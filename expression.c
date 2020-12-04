#include "pacc.h"


static Node read_struct_field(parser p, index offset, Node struc) {
    // or union?
    Type ty = pget(struc, sym(type));
    if (pget(ty, sym(kind)) != sym(struct))
        error("struct expected, but got %s", node2s(struc));
    tuple name = token(p, offset);
    if (pget(name, sym(kind)) != sym(identifier))
        error("field name expected, but got %s", name);
    Type field = lookup_field(ty, pget(name, sym(value)));
    if (!field)
        error("struct has no such field: %s", name);
    
    return timm("kind", sym(struct_ref),
                "type", field,
                "struct", struc,
                "field", pget(name, sym(value)));
}

static Node ast_unaryop(string kind, Type ty, Node operand) {
    return timm("kind", kind, "type", ty, "operand", operand);
}

static boolean same_arith_type(Type t, Type u) {
    return toboolean(get(t, sym(kind)) == get(u, sym(kind)) && get(t, sym(usig)) == get(u, sym(usig)));
}

static Node wrap(Type t, Node node) {
    if (same_arith_type(t, get(node, "type")))
        return node;
    return ast_unaryop(sym(conv), t, node);
}


Node conv(parser p, Node node) {
    Type int_type = pget(p->global, sym(type), sym(int)); // ?
    Type ty = get(node, sym(type));
    string kind = get(ty, sym(kind));
    if (kind == sym(array))
        // c11 6.3.2.1p3: an array of t is converted to a pointer to t.
        return ast_unaryop(sym(conv), make_ptr_type(get(ty, sym(ptr))), node);
    if (kind == sym(func))
        // c11 6.3.2.1p4: a function designator is converted to a pointer to the function.
        return ast_unaryop(sym(addr), make_ptr_type(ty), node);

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

// shouldn't this fall out?
static Type read_typeof(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    Type r = is_type(p, token(p, offset))
        ? read_cast_type(p, offset, env)
        : pget(read_expr(p, env), sym(type));
    expect(p, offset, stringify(")"));
    return r;
}

static boolean is_poweroftwo(int x) {
    // If there's only one bit set in x, the value is a power of 2.
    return toboolean((x <= 0) ? 0 : !(x & (x - 1)));
}

static vector read_func_args(parser p, index offset, scope env, vector params) {
    vector args = 0; // larvate?
    int i = 0;
    while (!next_token(p, offset, stringify(")"))) {
        Node arg = conv(p, read_assignment_expr(p, offset, env));
        Type ty = pget(arg, sym(type));

#if 0        
        Type paramtype;
        // why dont we just unify this later?
        if (!(paramtype = pget(params, i++))) {
            // default types?
            paramtype =
                is_inttype(ty) ? pget(p->global, sym(types), sym(int)) :
                pget(arg, sym(type));
        }
#endif
        
        // ensure_assignable(paramtype, ty);
        // maybe later 
        //        if (pget(paramtype, sym(kind)) != pget(arg, sym(type), sym(kind)))
        //            arg = ast_conv(paramtype, arg);

        //args = push(args, arg);

        tuple tok = token(p, offset);
        if (is_keyword(tok, stringify(")"))) break;
        if (!is_keyword(tok, stringify(",")))
            error(p, "unexpected token: '%s'", tok);
    }
    return args;
}


static Node read_funcall(parser p, index offset, scope env, Node fp) {
    if (pget(fp, sym(kind)) == sym(addr) && pget(fp, sym(operand), sym(kind)) == sym(funcdesg)) {
        Node desg = pget(fp, sym(operand));
        vector args = read_func_args(p, offset, env, pget(desg, sym(type), sym(parameters)));
        //        ast_funcall(Type ftype, buffer fname, vector args)
        return timm("kind", sym(funcall),
                    "type", pget(desg, sym(type), sym(rettype)), // rettype
                    "name", pget(desg, sym(name)),
                    "args", args,
                    "ftype", pget(desg, sym(type))); // we need this why?
    }
    vector args = read_func_args(p, offset, env, pget(fp, sym(type), sym(ptr), sym(parameters)));
    // this is not a separate thing    
    //    ast_funcptr_call(fp, args);
    return timm("kind", sym(funcptr_call), "type",
                pget(fp, sym(type), sym(ptr), sym(rettype)),
                "fptr", fp, 
                "args", args);
}

static Node read_postfix_expr_tail(parser p, index offset, scope env, Node node) {
    if (next_token(p, offset, stringify("("))) {
        node = conv(p, node);
        Type t = pget(node, sym(type));
        if (pget(node, sym(kind)) != sym(ptr) || pget(t, sym(ptr), sym(kind) != sym(func)))
            error(p, "function expected, but got %s", node);
        node = read_funcall(p, offset, env, node);
    }
    if (next_token(p, offset, stringify("["))) {
        node = read_subscript_expr(p, offset, env, node);
    }
    // why dont these look more like binary operators?
    if (next_token(p, offset, sym(.))) {
        node = read_struct_field(p, offset, node);
    }
    if (next_token(p, offset, sym(->))) {
        if (pget(node, sym(type), sym(kind)) != sym(ptr))
            error("pointer type expected, but got %s %s",
                  ty2s(node->ty), node2s(node));
        node = ast_unaryop(sym(deref), pget(node, sym(type), sym(ptr)), node);
        node = read_struct_field(p, offset, node);
    }
    tuple tok = token(p, offset);
    if (next_token(p, offset, sym(inc)) || next_token(p, offset, sym(dec))) {
        string op = is_keyword(tok, sym(inc)) ? sym(post_inc) : sym(post_dec);
        return ast_unaryop(op, pget(node, sym(type)), node);
    }
    return node;
}

static Node read_unary_expr(parser p, index offset, scope env) ;


static vector read_decl_init(parser p, index offset, scope env, Type ty) {
    vector r = 0;
    if (is_keyword(token(p, offset), stringify("{")) || is_string(ty)) {
        read_initializer_list(p, offset, env, r, ty, false);
    } else {
        Node init = conv(p, read_assignment_expr(p, offset, env));
        if (is_inttype(pget(init, sym(type))) && pget(init, sym(type), sym(kind)) != pget(ty, sym(kind)))
            init = ast_conv(ty, init);
        // push(r, ast_init(init, ty));
    }
    return r;
}

static Node read_compound_literal(parser p, index offset, scope env, Type ty) {
    buffer name = make_label();
    vector init = read_decl_init(p, offset, env, ty);
    Node r = ast_var(env, ty, name);
    // set(r, sym(init), init);
    return r;
}


Node read_cast_expr(parser p, index offset, scope env) {
    tuple tok = token(p, offset);
    if (is_keyword(tok, stringify("(")) && is_type(p, token(p, offset+1))) {
        Type ty = read_cast_type(p, offset, env);
        expect(p, offset + 2, stringify(")"));
        if (is_keyword(token(p, offset + 3), stringify("}"))) {
            Node node = read_compound_literal(p, offset, env, ty);
            return read_postfix_expr_tail(p, offset, env, node);
        }
        return ast_unaryop(sym(cast), ty, read_cast_expr(p, offset, env));
    }
    return read_unary_expr(p, offset, env);
}

static Node read_multiplicative_expr(parser p, index offset, scope env) {
    Node node = read_cast_expr(p, offset, env);

    // x in {*, /, %}
    if (next_token(p, offset, sym(*)))
        node = binop(p, sym(*), conv(p, node), conv(p, read_cast_expr(p, offset, env)));
    else if (next_token(p, offset, sym(/)))
        node = binop(p, sym(/), conv(p, node), conv(p, read_cast_expr(p, offset, env)));
    else if (next_token(p, offset, sym(%)))
        node = binop(p, sym(%), conv(p, node), conv(p, read_cast_expr(p, offset, env)));
    else  return node;
}

static Node read_additive_expr(parser p, index offset, scope env) {
    Node node = read_multiplicative_expr(p, offset, env);
    if (next_token(p, offset, sym(+)))
        node = binop(p, sym(+), conv(p, node), conv(p, read_multiplicative_expr(p, offset, env)));
    else if (next_token(p, offset, sym(-)))
        node = binop(p, sym(-), conv(p, node), conv(p, read_multiplicative_expr(p, offset, env)));
    else return node;
}

static Node read_shift_expr(parser p, index offset, scope env) {
    Node node = read_additive_expr(p, offset, env);

    string op;
    if (next_token(p, offset, sym(<<)))
        op = sym(<<);
    else if (next_token(p, offset, sym(>>)))
        op = sym(>>);
    else return offset;
        
    Node right = read_additive_expr(p, offset, env);
    node = ast_binop(p, pget(node, sym(type)), op, conv(p, node), conv(p, right));
    return node;
}

// ordering 
static Node read_relational_expr(parser p, index offset, scope env) {
    Node node = read_shift_expr(p, offset, env);
    // we used to set the type to int if its not handled here...seems quite wrong
    //        value ty = pget(p->global, sym(type), sym(int));
    // flatten
    if  (next_token(p, offset, sym(<)))      return binop(p, sym(<),  conv(p, node), conv(p, read_shift_expr(p, offset, env)));
    else if (next_token(p, offset, sym(>)))  return binop(p, sym(>),  conv(p, read_shift_expr(p, offset, env)), conv(p, node));
    else if (next_token(p, offset, sym(<=))) return binop(p, sym(<=), conv(p, node), conv(p, read_shift_expr(env, offset, p)));
    else if (next_token(p, offset, sym(>=))) return binop(p, sym(>=), conv(p, read_shift_expr(p, offset, env)), conv(p, node));
    else  return node;

}

static Node read_equality_expr(parser p, index offset, scope env) {
    Node node = read_relational_expr(p, offset, env);
    Node r;
    if (next_token(p, offset, sym(==))) {
        r = binop(p, sym(==), conv(p, node), conv(p, read_equality_expr(p, offset, env)));
    } else if (next_token(p, offset, sym(!=))) {
        r = binop(p, sym(!=), conv(p, node), conv(p, read_equality_expr(p, offset, env)));
    } else {
        return node;
    }
    // ok - yes, we know this is a boolean. binop _should_ take a type? 
    // set(r, sym(type), pget(p->global, sym(type), sym(int)));
    return r;
}

// this is precidence....i'd rather railroad - can we do that pure?
static Node read_bitand_expr(parser p, index offset, scope env) {
    Node node = read_equality_expr(p, offset, env);
    while (next_token(p, offset, sym(&)))
        node = binop(p, sym(&), conv(p, node), conv(p, read_equality_expr(p, offset, env)));
    return node;
}

static Node read_bitxor_expr(parser p, index offset, scope env) {
    Node node = read_bitand_expr(p, offset, env);
    while (next_token(p, offset, sym(^)))
        node = binop(p, sym(^), conv(p, node), conv(p, read_bitand_expr(p, offset, env)));
    return node;
}

static Node read_bitor_expr(parser p, index offset, scope env) {
    Node node = read_bitxor_expr(p, offset, env);
    while (next_token(p, offset, sym(|)))
        node = binop(p, sym(|), conv(p, node), conv(p, read_bitxor_expr(p, offset, env)));
    return node;
}

static Node read_logand_expr(parser p, index offset, scope env) {
    Node node = read_bitor_expr(p, offset, env);
    while (next_token(p, offset, sym(&&)))
        node = ast_binop(p, pget(p->global, sym(type), sym(int)), sym(&&), node, read_bitor_expr(p, offset, env));
    return node;
}

static Node read_logor_expr(parser p, index offset, scope env) {
    Node node = read_logand_expr(p, offset, env);
    while (next_token(p, offset, sym(||)))
        node = ast_binop(p, pget(p->global, sym(type), sym(int)), sym(||), node, read_logand_expr(p, offset, env));
    return node;
}

static Node read_conditional_expr(parser p, index offset,  scope env);

static Node ast_ternary(Type ty, Node cond, Node then, Node els)
{
    return timm("kind", sym(ternary), "cond", cond, "then", then, "else", els);
}

static Node do_read_conditional_expr(parser p, index offset, scope env, Node cond) {
    Node then = conv(p, read_expr(p, offset, env));
    expect(p, offset, sym(:));
    Node els = conv(p, read_conditional_expr(p, offset, env));
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

static Node read_conditional_expr(parser p, index offset, scope env) {
    Node cond = read_logor_expr(p, offset, env);
    if (!next_token(p, offset, sym(?)))
        return cond;
    return do_read_conditional_expr(p, offset, env, cond);
}

static string get_compound_assign_op(tuple tok) {
    if (pget(tok, sym(kind)) != sym(keyword))
        return 0;
    return(pget(tok, sym(id)));
}


Node read_assignment_expr(parser p, index offset, scope env) {
    Node node = read_logor_expr(p, offset, env);
    tuple tok = token(p, offset);
    if (!tok) return node;
    if (is_keyword(tok, sym(?)))
        return do_read_conditional_expr(p, offset, env, node);
    string cop = get_compound_assign_op(tok);
    if (is_keyword(tok, sym(=)) || cop) {
        Node value = conv(p, read_assignment_expr(p, offset, env));
        Node right = cop ? binop(p, cop, conv(p, node), value) : value;
        Type ty = pget(node, sym(type));
        if (is_inttype(ty) && pget(ty, sym(kind)) != pget(right, sym(ty), sym(kind)))
            right = ast_conv(ty, right);
        return ast_binop(p, ty, sym(=), node, right);
    }
    return node;
}


// dont simplify this?
Node read_subscript_expr(parser p, index offset, scope env, Node node) {
    tuple tok = token(p, offset);
    Node sub = read_expr(p, offset, env);
    if (!sub) error(p, "subscript expected");
    expect(p, offset, stringify("]"));
    Node t = binop(p, sym(+), conv(p, node), conv(p, sub));
    return ast_unaryop(sym(deref), pget(t, sym(type), sym(ptr)), t);
}


// was read comma_expr
Node read_expr(parser p, index offset, scope env) {
    Node node = read_assignment_expr(p, offset, env);
    while (next_token(p, offset, stringify(","))){
        Node expr = read_assignment_expr(p, offset, env);
        node = ast_binop(p, pget(expr, sym(type)), stringify(","), node, expr);
    }
    return node;
}


static Node read_unary_incdec(parser p, index offset, scope env, string op) {
    Node operand = read_unary_expr(p, offset, env);
    operand = conv(p, operand);
    return ast_unaryop(op, pget(operand, sym(type)), operand);
}

static Node read_label_addr(parser p, index offset, tuple tok) {
    // [GNU] Labels as values. You can get the address of the a label
    // with unary "&&" operator followed by a label name.
    tuple tok2 = token(p, offset);
    if (pget(tok2, sym(kind)) != sym(identifier))
        error(p, "label name expected after &&, but got %s", tok2);
    // type void
    Node r = timm("kind", sym(label_addr), "type",
                  make_ptr_type(pget(p->global, sym(type), sym(void))),
                  "name", pget(tok2, sym(value)));
    
    // push(pget(env, sym(gotos)), r);
    return r;
}

static Node read_unary_addr(parser p, index offset, scope env) {
    Node operand = read_cast_expr(p, offset, env);
    if (pget(operand, sym(kind)) == sym(funcdesg))
        return conv(p, operand);
    return ast_unaryop(sym(addr), make_ptr_type(pget(operand, sym(type))), operand);
}

static Node read_unary_deref(parser p, index offset, scope env, tuple tok) {
    Node operand = conv(p, read_cast_expr(p, offset, env));
    Type ot = pget(operand, sym(type));
    if (pget(ot,sym(kind)) != sym(ptr))
        error(p, "pointer type expected, but got %s", node2s(operand));
    if (pget(ot, sym(ptr), sym(kind)) == sym(func))
        return operand;
    return ast_unaryop(sym(deref), pget(ot, sym(ptr)), operand);
}

static Node read_unary_minus(parser p, index offset, scope env) {
    Node expr = read_cast_expr(p, offset, env);
    return binop(p, sym(-), conv(p, ast_int_literal(p, pget(expr, sym(type)), 0)), conv(p, expr));
}

static Node read_unary_bitnot(parser p, index offset, scope env, tuple tok) {
    Node expr = read_cast_expr(p, offset, env);
    expr = conv(p, expr);
    Type et = pget(expr, sym(type));
    if (!is_inttype(et))
        error(p, "invalid use of ~: %s", node2s(expr));
    return ast_unaryop(sym(~), et, expr);
}

static Node read_unary_lognot(parser p, index offset, scope env) {
    Node operand = read_cast_expr(p, offset, env);
    operand = conv(p, operand);
    return ast_unaryop(sym(!), pget(p->global, sym(type), sym(int)), operand);
}



static Node read_stmt_expr(parser p, index offset, scope env) {
    Node r = read_compound_stmt(p, offset, env);
    expect(p, offset, stringify(")"));
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

// fold 
static Node ast_funcdesg(Type ty, string fname)
{
    return timm("kind", sym(funcdesg), "tupe", ty, "fname", fname);
}

static Node read_var_or_func(parser p, index offset, scope env, buffer name) {
    Node v = pget(env, name);
    if (!v) {
        tuple tok = token(p, offset);
        if (!is_keyword(tok, stringify("(")))
            error(p, "undefined variable: %s", name);
        Type ty = make_func_type(pget(p->global, sym(type), sym(int)), 0);
        // warnt(tok, "assume returning int: %s()", name);
        return ast_funcdesg(ty, name);
    }
    // so..funcdesg is really just a variable of function type?
    if (pget(v, sym(type), sym(kind)) == sym(func))
        return ast_funcdesg(pget(v, sym(type)), name);
    return v;
}

static Node read_primary_expr(parser p, index offset, scope env) {
    tuple tok = token(p, offset);
    // if (!tok) return zero;
    value k = pget(tok, sym(kind));
    value v = pget(tok, sym(value));
    if (is_keyword(tok, stringify("("))) {
        if (next_token(p, offset, stringify("[")))
            return read_stmt_expr(p, offset, env);
        Node r = read_expr(p, offset, env);
        expect(p, offset, stringify(")"));
        return r;
    }
    if (k == sym(keyword)) return zero;
    if (k == sym(identifier)) return read_var_or_func(p, offset, env, v);
    //  if (tok->kind == sym(number))
    //    return read_int(p, tok);
    if (k == sym(char))  return ast_int_literal(p, pget(p->global, sym(type), sym(char)), v);
    if (k == sym(string)) return ast_string(p, env, v);
    error(p, "internal error: unknown token kind: %d", k);
    return 0;
}



static Node read_sizeof_operand(parser p, index offset, scope env) {
    tuple tok = get(p->lex, (value)offset);
    if (is_keyword(tok, stringify("(")) && is_type(p, token(p, offset))) {
        Type r = read_cast_type(p, offset, env);

        expect(p, offset, stringify(")"));
        return r;
    }    
    Type ty = pget(read_unary_expr(p, offset, env), sym(type));
    string tyk = pget(ty, sym(kind));
    // Sizeof on void or function type is GNU extension
    value size = (tyk == sym(void) || tyk == sym(func)) ?
        value_from_u64(1) : pget(ty, sym(size));
    return ast_int_literal(p, pget(p->global, sym(type), sym(ulong)), size);
}

// do we really .. want .. alignof?
static Node read_alignof_operand(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    Type ty = read_cast_type(p, offset, env);
    expect(p, offset, stringify(")"));
    return ast_int_literal(p, pget(p->global, sym(types), sym(ulong)),
                       pget(ty, 0));
}

static Node read_unary_expr(parser p, index offset, scope env) {
    tuple tok = get_token(p->lex);
    if (pget(tok, sym(kind)) == sym(keyword)) {
        value id = pget(tok, sym(id));
        if (id == sym(sizeof)) return read_sizeof_operand(p, offset, env);
        if (id == sym(alignof)) return read_alignof_operand(p, offset, env);
        if (id == sym(pre_inc)) return read_unary_incdec(p, offset, env, sym(pre_inc));
        if (id == sym(pre_dec)) return read_unary_incdec(p, offset, env, sym(pre_dec));
        // computed goto?
        if (id == sym(&&)) return read_label_addr(p, offset, tok);
        if (id == sym(&)) return read_unary_addr(p, offset, env);
        if (id == sym(*)) return read_unary_deref(p, offset, env, tok);
        if (id == sym(+)) return read_cast_expr(p, offset, env);
        if (id == sym(-)) return read_unary_minus(p, offset, env);
        if (id == sym(~)) return read_unary_bitnot(p,offset,  env, tok);
        if (id == sym(!)) return read_unary_lognot(p, offset, env);
    }
    return read_postfix_expr_tail(p, offset, env, read_primary_expr(p, offset, env));
}

