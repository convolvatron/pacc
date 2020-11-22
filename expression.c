#include "pacc.h"

static Node wrap(Type t, Node node) {
    if (same_arith_type(t, get(node, "type")))
        return node;
    return ast_uop(sym(conv), t, node);
}

static Node ast_uop(string kind, Type ty, Node operand) {
    return timm("kind", kind, "type", ty, "operand", operand);
}

Node conv(parser p, Node node) {
    Type int_type = pget(p->global, sym(type), sym(int)); // ?
    Type ty = get(node, sym(type));
    string kind = get(ty, sym(kind));
    if (kind == sym(array))
        // c11 6.3.2.1p3: an array of t is converted to a pointer to t.
        return ast_uop(sym(conv), make_ptr_type(get(ty, sym(ptr))), node);
    if (kind == sym(func))
        // c11 6.3.2.1p4: a function designator is converted to a pointer to the function.
        return ast_uop(sym(addr), make_ptr_type(ty), node);

    // generalize
    if ((kind == sym(short)) ||
        (kind == sym(char)) ||
        (kind == sym(boolean)))
        // c11 6.3.1.1p2: the integer promotions
        return ast_conv(int_type, node);
    if (kind == sym(int))
        if (get(ty, sym(bitsize)))
            return ast_conv(int_type, node);
    return node;
}

// c11 6.3.1.8: usual arithmetic conversions
static Type usual_arith_conv(parser p, Type t, Type u) {
    // umm .. uh oh
    //  if (t->kind < u->kind) {
    //    // Make t the larger type
    //    Type tmp = t;
    //    t = u;
    //    u = tmp;
    //  }
    int ts = u64_from_value(get(t, sym(size)));
    int us = u64_from_value(get(u, sym(size)));
    if (ts > us)
        return t;
    if (get(t, sym(signed)) == get(u, sym(signed)))
        return t;
    return allocate_scope(t, sym(signed), sym(true));
}

// op property .. or a set i guess
static boolean valid_pointer_binop(string op) {
    if (op == stringify("-") ||
        op == stringify("<") ||
        op == stringify(">") ||
        op == stringify("<=") ||
        op == stringify(">=") ||
        op == stringify("=") ||
        op == stringify("!"))
        return true;
    return false;
}


string make_tempname() {
    static int c = 0;
    u8 staging[20];
    return allocate_utf8(staging, sprintf((char *)staging, ".T%d", c++)); // xx - libc
}

// from the left and the right? 
static Node binop(parser p, string op, Node lhs, Node rhs) {
    if (pget(lhs, sym(type), sym(kind)) == sym(ptr) &&
        pget(rhs, sym(type), sym(kind)) == sym(ptr)) {
        if (!valid_pointer_binop(op))
            error("invalid pointer arith");
        // C11 6.5.6.9: Pointer subtractions have type ptrdiff_t.
        if (op == sym(-))
            return ast_binop(p, pget(p->global, sym(types), sym(long)), op, lhs, rhs);
        // C11 6.5.8.6, 6.5.9.3: Pointer comparisons have type int.
        return ast_binop(p, pget(p->global, sym(types), sym(int)), op, lhs, rhs);
    }
    Type lt = get(lhs, sym(type));
    Type rt = get(rhs, sym(type));
    if (get(lt, sym(kind)) == sym(ptr))
        return ast_binop(p, lt, op, lhs, rhs);
    if (get(rt, sym(kind)) == sym(ptr))
        return ast_binop(p, rt, op, rhs, lhs);
    Type r = usual_arith_conv(p, lt, rt);
    return ast_binop(p, r, op, wrap(r, lhs), wrap(r, rhs));
}

static Type read_typeof(parser p, scope env) {
    expect(p, stringify("("));
    Type r = is_type(p, token(p))
        ? read_cast_type(p, env)
        : pget(read_expr(p, env), sym(type));
    expect(p, stringify(")"));
    return r;
}

static boolean is_poweroftwo(int x) {
    // If there's only one bit set in x, the value is a power of 2.
    return toboolean((x <= 0) ? 0 : !(x & (x - 1)));
}


static Node read_postfix_expr_tail(parser p, scope env, Node node) {
    if (!node) return zero;
    for (;;) {
        if (next_token(p, stringify("("))) {
            node = conv(p, node);
            Type t = pget(node, sym(type));
            if (pget(node, sym(kind)) != sym(ptr) || pget(t, sym(ptr), sym(kind) != sym(func)))
                error(p, "function expected, but got %s", node);
            node = read_funcall(p, env, node);
            continue;
        }
        if (next_token(p, stringify("["))) {
            node = read_subscript_expr(p, env, node);
            continue;
        }
        // why dont these look more like binary operators?
        if (next_token(p, sym(.))) {
            node = read_struct_field(p, node);
            continue;
        }
        if (next_token(p, sym(->))) {
            if (pget(node, sym(type), sym(kind)) != sym(ptr))
                error("pointer type expected, but got %s %s",
                      ty2s(node->ty), node2s(node));
            node = ast_uop(sym(deref), pget(node, sym(type), sym(ptr)), node);
            node = read_struct_field(p, node);
            continue;
        }
        tuple tok = token(p);
        if (next_token(p, sym(inc)) || next_token(p, sym(dec))) {
            //            ensure_lvalue(node);
            string op = is_keyword(tok, sym(inc)) ? sym(post_inc) : sym(post_dec);
            return ast_uop(op, pget(node, sym(type)), node);
        }
        return node;
    }
}
static Node read_unary_expr(parser p, scope env) ;

static Node read_cast_expr(parser p, scope env) {
    tuple tok = token(p);
    if (is_keyword(tok, stringify("(")) && is_type(p, token(p))) {
        Type ty = read_cast_type(p, env);
        expect(p, stringify(")"));
        if (is_keyword(token(p), stringify("}"))) {
            Node node = read_compound_literal(p, env, ty);
            return read_postfix_expr_tail(p, env, node);
        }
        return ast_uop(sym(cast), ty, read_cast_expr(p, env));
    }
    unget(p, tok);
    return read_unary_expr(p, env);
}

static Node read_multiplicative_expr(parser p, scope env) {
    Node node = read_cast_expr(p, env);
    for (;;) {
        if (next_token(p, sym(*)))   node = binop(p, sym(*), conv(p, node), conv(p, read_cast_expr(p, env)));
        else if (next_token(p, sym(/))) node = binop(p, sym(/), conv(p, node), conv(p, read_cast_expr(p, env)));
        else if (next_token(p, sym(%))) node = binop(p, sym(%), conv(p, node), conv(p, read_cast_expr(p, env)));
        else  return node;
    }
}

static Node read_additive_expr(parser p, scope env) {
    Node node = read_multiplicative_expr(p, env);
    for (;;) {
        if (next_token(p, sym(+))) node = binop(p, sym(+), conv(p, node), conv(p, read_multiplicative_expr(p, env)));
        else if (next_token(p, sym(-))) node = binop(p, sym(-), conv(p, node), conv(p, read_multiplicative_expr(p, env)));
        else return node;
    }
}

static Node read_shift_expr(parser p, scope env) {
    Node node = read_additive_expr(p, env);
    for (;;) {
        string op;
        if (next_token(p, sym(<<)))
            op = sym(<<);
        else if (next_token(p, sym(>>)))
            op = sym(>>);
        else
            break;
        Node right = read_additive_expr(p, env);
        node = ast_binop(p, pget(node, sym(type)), op, conv(p, node), conv(p, right));
    }
    return node;
}

static Node read_relational_expr(parser p, scope env) {
    Node node = read_shift_expr(p, env);
    for (;;) {
        // we used to set the type to int if its not handled here...seems quite wrong
        //        value ty = pget(p->global, sym(type), sym(int));
        // flatten
        if  (next_token(p, sym(<)))      return binop(p, sym(<),  conv(p, node), conv(p, read_shift_expr(p, env)));
        else if (next_token(p, sym(>)))  return binop(p, sym(>),  conv(p, read_shift_expr(p, env)), conv(p, node));
        else if (next_token(p, sym(<=))) return binop(p, sym(<=), conv(p, node), conv(p, read_shift_expr(env, p)));
        else if (next_token(p, sym(>=))) return binop(p, sym(>=), conv(p, read_shift_expr(p, env)), conv(p, node));
        else  return node;
    }
}

static Node read_equality_expr(parser p, scope env) {
    Node node = read_relational_expr(p, env);
    Node r;
    if (next_token(p, sym(==))) {
        r = binop(p, sym(==), conv(p, node), conv(p, read_equality_expr(p, env)));
    } else if (next_token(p, sym(!=))) {
        r = binop(p, sym(!=), conv(p, node), conv(p, read_equality_expr(p, env)));
    } else {
        return node;
    }
    // ok - yes, we know this is a boolean. binop _should_ take a type? 
    // set(r, sym(type), pget(p->global, sym(type), sym(int)));
    return r;
}

// this is precidence....i'd rather railroad
static Node read_bitand_expr(parser p, scope env) {
    Node node = read_equality_expr(p, env);
    while (next_token(p, sym(&)))
        node = binop(p, sym(&), conv(p, node), conv(p, read_equality_expr(p, env)));
    return node;
}

static Node read_bitxor_expr(parser p, scope env) {
    Node node = read_bitand_expr(p, env);
    while (next_token(p,sym(^)))
        node = binop(p, sym(^), conv(p, node), conv(p, read_bitand_expr(p, env)));
    return node;
}

static Node read_bitor_expr(parser p, scope env) {
    Node node = read_bitxor_expr(p, env);
    while (next_token(p, sym(|)))
        node = binop(p, sym(|), conv(p, node), conv(p, read_bitxor_expr(p, env)));
    return node;
}

static Node read_logand_expr(parser p, scope env) {
    Node node = read_bitor_expr(p, env);
    while (next_token(p, sym(&&)))
        node = ast_binop(p, pget(p->global, sym(type), sym(int)), sym(&&), node, read_bitor_expr(p, env));
    return node;
}

static Node read_logor_expr(parser p, scope env) {
    Node node = read_logand_expr(p, env);
    while (next_token(p, sym(||)))
        node = ast_binop(p, pget(p->global, sym(type), sym(int)), sym(||), node, read_logand_expr(p, env));
    return node;
}

static Node read_conditional_expr(parser p, scope env);

static Node ast_ternary(Type ty, Node cond, Node then, Node els)
{
    return timm("kind", sym(ternary), "cond", cond, "then", then, "else", els);
}

static Node do_read_conditional_expr(parser p, scope env, Node cond) {
    Node then = conv(p, read_expr(p, env));
    expect(p, sym(:));
    Node els = conv(p, read_conditional_expr(p, env));
    // [GNU] Omitting the middle operand is allowed.
    Type t = then ? pget(then, sym(type)): pget(cond, sym(type));
    Type u = pget(els, sym(ty));
    // C11 6.5.15p5: if both types are arithemtic type, the result
    // type is the result of the usual arithmetic conversions.
    if (is_inttype(t) && is_inttype(u)) {
        Type r = usual_arith_conv(p, t, u);
        return ast_ternary(r, cond, (then ? wrap(r, then) : zero), wrap(r, els));
    }
    return ast_ternary(u, cond, then, els);
}

static Node read_conditional_expr(parser p, scope env) {
    Node cond = read_logor_expr(p, env);
    if (!next_token(p, sym(?)))
        return cond;
    return do_read_conditional_expr(p, env, cond);
}

static Node read_assignment_expr(parser p, scope env) {
    Node node = read_logor_expr(p, env);
    tuple tok = token(p);
    if (!tok) return node;
    if (is_keyword(tok, sym(?)))
        return do_read_conditional_expr(p, env, node);
    string cop = get_compound_assign_op(tok);
    if (is_keyword(tok, sym(=)) || cop) {
        Node value = conv(p, read_assignment_expr(p, env));
        if (is_keyword(tok, sym(=)) || cop)
            ensure_lvalue(node);
        Node right = cop ? binop(p, cop, conv(p, node), value) : value;
        Type ty = pget(node, sym(type));
        if (is_inttype(ty) && pget(ty, sym(kind)) != pget(right, sym(ty), sym(kind)))
            right = ast_conv(ty, right);
        return ast_binop(p, ty, sym(=), node, right);
    }
    // unget_token(tok);
    return node;
}


Node read_subscript_expr(parser p, scope env, Node node) {
    tuple tok = token(p);
    Node sub = read_expr(p, env);
    if (!sub) error(p, "subscript expected");
    expect(p, stringify("]"));
    Node t = binop(p, sym(+), conv(p, node), conv(p, sub));
    return ast_uop(sym(deref), pget(t, sym(type), sym(ptr)), t);
}


// was read comma_expr
static Node read_expr(parser p, scope env) {
    Node node = read_assignment_expr(p, env);
    while (next_token(p, stringify(","))){
        Node expr = read_assignment_expr(p, env);
        node = ast_binop(p, pget(expr, sym(type)), stringify(","), node, expr);
    }
    return node;
}


static Node read_unary_incdec(parser p, scope env, string op) {
    Node operand = read_unary_expr(p, env);
    operand = conv(p, operand);
    ensure_lvalue(operand);
    return ast_uop(op, pget(operand, sym(type)), operand);
}

static Node read_label_addr(parser p, tuple tok) {
    // [GNU] Labels as values. You can get the address of the a label
    // with unary "&&" operator followed by a label name.
    tuple tok2 = token(p);
    if (pget(tok2, sym(kind)) != sym(identifier))
        error(p, "label name expected after &&, but got %s", tok2);
    Node r = ast_label_addr(p, pget(tok2, sym(value)));
    // push(pget(env, sym(gotos)), r);
    return r;
}

static Node read_cast_expr(parser p, scope env);

static Node read_unary_addr(parser p, scope env) {
    Node operand = read_cast_expr(p, env);
    if (pget(operand, sym(kind)) == sym(funcdesg))
        return conv(p, operand);
    ensure_lvalue(operand);
    return ast_uop(sym(addr), make_ptr_type(pget(operand, sym(type))), operand);
}

static Node read_unary_deref(parser p, scope env, tuple tok) {
    Node operand = conv(p, read_cast_expr(p, env));
    Type ot = pget(operand, sym(type));
    if (pget(ot,sym(kind)) != sym(ptr))
        error(p, "pointer type expected, but got %s", node2s(operand));
    if (pget(ot, sym(ptr), sym(kind)) == sym(func))
        return operand;
    return ast_uop(sym(deref), pget(ot, sym(ptr)), operand);
}

static Node read_unary_minus(parser p, scope env) {
    Node expr = read_cast_expr(p, env);
    return binop(p, sym(-), conv(p, ast_inttype(p, pget(expr, sym(type)), 0)), conv(p, expr));
}

static Node read_unary_bitnot(parser p, scope env, tuple tok) {
    Node expr = read_cast_expr(p, env);
    expr = conv(p, expr);
    Type et = pget(expr, sym(type));
    if (!is_inttype(et))
        error(p, "invalid use of ~: %s", node2s(expr));
    return ast_uop(sym(~), et, expr);
}

static Node read_unary_lognot(parser p, scope env) {
    Node operand = read_cast_expr(p, env);
    operand = conv(p, operand);
    return ast_uop(sym(!), pget(p->global, sym(type), sym(int)), operand);
}



static Node read_stmt_expr(parser p, scope env) {
    Node r = read_compound_stmt(p, env);
    expect(p, stringify(")"));
    Type rtype = pget(p->global, sym(type), sym(void));

    value st;
    if ((st = pget(r, "statements"))) {
        Node lastexpr = vector_peek(st);
        value v;
        if ((v = pget(lastexpr, sym(type))))
            rtype = v;
    }
    // evert if possible
    return allocate_scope(r, sym(type), rtype);
}

static Node read_primary_expr(parser p, scope env) {
    tuple tok = token(p);
    // if (!tok) return zero;
    value k = pget(tok, sym(kind));
    value v = pget(tok, sym(value));
    if (is_keyword(tok, stringify("("))) {
        if (next_token(p, stringify("[")))
            return read_stmt_expr(p, env);
        Node r = read_expr(p, env);
        expect(p, stringify(")"));
        return r;
    }
    if (k == sym(keyword)) return zero;
    if (k == sym(identifier)) return read_var_or_func(p, env, v);
    //  if (tok->kind == sym(number))
    //    return read_int(p, tok);
    if (k == sym(char))  return ast_inttype(p, pget(p->global, sym(type), sym(char)), v);
    if (k == sym(string)) return ast_string(p, env, v);
    error(p, "internal error: unknown token kind: %d", k);
    return 0;
}



static Node read_sizeof_operand(parser p, scope env) {
    tuple tok = get_token(p->lex);
    if (is_keyword(tok, stringify("(")) && is_type(p, token(p))) {
        Type r = read_cast_type(p, env);

        expect(p, stringify(")"));
        return r;
    }    
    Type ty = pget(read_unary_expr(p, env), sym(type));
    string tyk = pget(ty, sym(kind));
    // Sizeof on void or function type is GNU extension
    value size = (tyk == sym(void) || tyk == sym(func)) ?
        value_from_u64(1) : pget(ty, sym(size));
    return ast_inttype(p, pget(p->global, sym(type), sym(ulong)), size);
}

// do we really .. want .. alignof?
static Node read_alignof_operand(parser p, scope env) {
    expect(p, stringify("("));
    Type ty = read_cast_type(p, env);
    expect(p, stringify(")"));
    return ast_inttype(p, pget(p->global, sym(types), sym(ulong)),
                       pget(ty, 0));
}

static Node read_unary_expr(parser p, scope env) {
    tuple tok = get_token(p->lex);
    if (pget(tok, sym(kind)) == sym(keyword)) {
        value id = pget(tok, sym(id));
        if (id == sym(sizeof)) return read_sizeof_operand(p, env);
        if (id == sym(alignof)) return read_alignof_operand(p, env);
        if (id == sym(pre_inc)) return read_unary_incdec(p, env, sym(pre_inc));
        if (id == sym(pre_dec)) return read_unary_incdec(p, env, sym(pre_dec));
        // computed goto?
        if (id == sym(&&)) return read_label_addr(p, tok);
        if (id == sym(&)) return read_unary_addr(p, env);
        if (id == sym(*)) return read_unary_deref(p, env, tok);
        if (id == sym(+)) return read_cast_expr(p, env);
        if (id == sym(-)) return read_unary_minus(p, env);
        if (id == sym(~)) return read_unary_bitnot(p, env, tok);
        if (id == sym(!)) return read_unary_lognot(p, env);
    }
    unget(p, tok);
    return read_postfix_expr_tail(p, env, read_primary_expr(p, env));
}


