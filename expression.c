#include "pacc.h"


static result read_struct_field(parser p, index offset, Node struc) {
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
    
    return res(timm("kind", sym(struct_ref),
                    "type", field,
                    "struct", struc,
                    "field", pget(name, sym(value))), offset);
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

#if 0
// later pass ? not at all?
result conv(scope env, result node) {
    Type int_type = pget(env, sym(type), sym(int));
    Type ty = get(node.v, sym(type));
    string kind = get(ty, sym(kind));
    if (kind == sym(array))
        // c11 6.3.2.1p3: an array of t is converted to a pointer to t.
        return res(ast_unaryop(sym(conv), make_ptr_type(get(ty, sym(ptr))), node.v), node.offset);
    if (kind == sym(func))
        // c11 6.3.2.1p4: a function designator is converted to a pointer to the function.
        return res(ast_unaryop(sym(addr), make_ptr_type(ty), node.v), node.offset);

    // generalize
    if ((kind == sym(short)) ||
        (kind == sym(char)) ||
        (kind == sym(boolean)))
        // c11 6.3.1.1p2: the integer promotions
        return res(ast_conv(int_type, node.v), node.offset);
    if (kind == sym(int))
        if (get(ty, sym(bitsize)))
            return res(ast_conv(int_type, node.v), node.offset);
    return node;
}
#endif

// c11 6.3.1.8: usual arithmetic conversions
static Type usual_arith_conv(Type t, Type u) {
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
static Node binop(scope env, string op, Node lhs, Node rhs) {
#if  0
    if (pget(lhs, sym(type), sym(kind)) == sym(ptr) &&
        pget(rhs, sym(type), sym(kind)) == sym(ptr)) {
        if (!valid_pointer_binop(op))
            error("invalid pointer arith");
        // C11 6.5.6.9: Pointer subtractions have type ptrdiff_t.
        if (op == sym(-))
            return ast_binop(pget(env, sym(types), sym(long)), op, lhs, rhs);
        // C11 6.5.8.6, 6.5.9.3: Pointer comparisons have type int.
        return ast_binop(pget(env, sym(types), sym(int)), op, lhs, rhs);
    }
#endif    
    Type lt = get(lhs, sym(type));
    Type rt = get(rhs, sym(type));
    if (get(lt, sym(kind)) == sym(ptr))   return ast_binop(lt, op, lhs, rhs);
    if (get(rt, sym(kind)) == sym(ptr))   return ast_binop(rt, op, rhs, lhs);
    value r = usual_arith_conv(lt, rt);
    return ast_binop(r, op, wrap(r, lhs), wrap(r, rhs));
}

// shouldn't this fall out?
static result read_typeof(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    value t;
    if (is_type(env, token(p, offset))) {
        result n = read_cast_type(p, offset, env);
        t = n.v;
    } else {
        result n = read_expr(p, offset, env);
        t = pget(n.v, sym(type));
    }
    expect(p, offset, stringify(")"));
    return res(t, offset);
}

static boolean is_poweroftwo(int x) {
    // If there's only one bit set in x, the value is a power of 2.
    return toboolean((x <= 0) ? 0 : !(x & (x - 1)));
}

static result read_func_args(parser p, index offset, scope env, vector params) {
    vector args = 0; // larvate?
    int i = 0;
    while (!next_token(p, offset, stringify(")"))) {
        result arg = read_assignment_expr(p, offset, env);
        Type ty = pget(arg.v, sym(type));

#if 0        
        Type paramtype;
        // why dont we just unify this later?
        if (!(paramtype = pget(params, i++))) {
            // default types?
            paramtype =
                is_inttype(ty) ? pget(env, sym(types), sym(int)) :
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
    return res(args, offset);
}


static result read_funcall(parser p, index offset, scope env, Node fp) {
    if (pget(fp, sym(kind)) == sym(addr) && pget(fp, sym(operand), sym(kind)) == sym(funcdesg)) {
        Node desg = pget(fp, sym(operand));
        result args = read_func_args(p, offset, env, pget(desg, sym(type), sym(parameters)));
        return res(timm("kind", sym(funcall),
                        "type", pget(desg, sym(type), sym(rettype)), // rettype
                        "name", pget(desg, sym(name)),
                        "args", args.v,
                        "ftype", pget(desg, sym(type))), // we need this why?
                   offset);
    }
    result args = read_func_args(p, offset, env, pget(fp, sym(type), sym(ptr), sym(parameters)));
    // this is not a separate thing    
    //    ast_funcptr_call(fp, args);
    return res(timm("kind", sym(funcptr_call), "type",
                    pget(fp, sym(type), sym(ptr), sym(rettype)),
                    "fptr", fp, 
                    "args", args.v), offset);
}

static result read_postfix_expr_tail(parser p, index offset, scope env, Node node) {
    result r;
    if (next_token(p, offset, stringify("("))) {
        // xxx 
        Type t = pget(node, sym(type));
        if (pget(node, sym(kind)) != sym(ptr) || pget(t, sym(ptr), sym(kind) != sym(func)))
            error(p, "function expected, but got %s", node);
        r = read_funcall(p, offset, env, node);
    }
    if (next_token(p, offset, stringify("["))) {
        r = read_subscript_expr(p, offset, env, node);
    }
    // why dont these look more like binary operators?
    if (next_token(p, offset, sym(.))) {
        r = read_struct_field(p, offset, node);
    }
    if (next_token(p, offset, sym(->))) {
        //        r = ast_unaryop(sym(deref), pget(node, sym(type), sym(ptr)), node);
        r = read_struct_field(p, offset, node);
    }
    tuple tok = token(p, offset);
    if (next_token(p, offset, sym(inc)) || next_token(p, offset, sym(dec))) {
        string op = is_keyword(tok, sym(inc)) ? sym(post_inc) : sym(post_dec);
        return res(ast_unaryop(op, pget(node, sym(type)), node), offset);
    }
    return r;
}

static result read_unary_expr(parser p, index offset, scope env) ;


result read_cast_expr(parser p, index offset, scope env) {
    tuple tok = token(p, offset);
    if (is_keyword(tok, stringify("(")) && is_type(p, token(p, offset+1))) {
        result ty = read_cast_type(p, offset, env);
        expect(p, offset + 2, stringify(")"));
        if (is_keyword(token(p, offset + 3), stringify("}"))) {
            // compound literal
            buffer name = make_label();
            vector init = read_decl_init(p, offset, env, ty.v);
            Node r = ast_var(env, ty.v, name);
            // set(r, sym(init), init);
            return read_postfix_expr_tail(p, offset, env, r);
        }
        return res(ast_unaryop(sym(cast), ty.v, read_cast_expr(p, offset, env).v), offset);
    }
    return read_unary_expr(p, offset, env);
}

static result read_multiplicative_expr(parser p, index offset, scope env) {
    result node = read_cast_expr(p, offset, env);
    result next = read_cast_expr(p, node.offset, env);
    value n;
    // x in {*, /, %}
    if (next_token(p, offset, sym(*)))  n = binop(env, sym(*), node.v, next.v);
    else if (next_token(p, offset, sym(/)))   n = binop(env, sym(/),  node.v, next.v);
    else if (next_token(p, offset, sym(%)))   n = binop(env, sym(%),  node.v, next.v);
    return res(n, offset);
}

static result read_additive_expr(parser p, index offset, scope env) {
    result node = read_multiplicative_expr(p, offset, env);
    result next = read_multiplicative_expr(p, node.offset, env);
    value n;
    if (next_token(p, offset, sym(+)))
        n = binop(env, sym(+),  node.v , next.v);
    else if (next_token(p, offset, sym(-)))
        n = binop(env, sym(-),  node.v , next.v);
    return node;
}

static result read_shift_expr(parser p, index offset, scope env) {
    result node = read_additive_expr(p, offset, env);

    string op;
    if (next_token(p, offset, sym(<<)))
        op = sym(<<);
    else if (next_token(p, offset, sym(>>)))
        op = sym(>>);
    else return res(zero, offset);
        
    result right = read_additive_expr(p, offset, env);
    return res(ast_binop(pget(node.v, sym(type)), op, node.v, right.v), offset);
}

// ordering 
static result read_relational_expr(parser p, index offset, scope env) {
    result node = read_shift_expr(p, offset, env);
    // we used to set the type to int if its not handled here...seems quite wrong
    //        value ty = pget(env, sym(type), sym(int));
    // flatten into a set
    result next = read_shift_expr(p, offset, env);
    Node n = node.v;
    if  (next_token(p, offset, sym(<)))      n = binop(env, sym(<),  node.v, next.v);
    else if (next_token(p, offset, sym(>)))  n = binop(env, sym(>),  next.v, node.v);
    else if (next_token(p, offset, sym(<=))) n = binop(env, sym(<=), node.v, next.v);
    else if (next_token(p, offset, sym(>=))) n = binop(env, sym(>=), next.v, node.v);
    return res(n, offset);
}

static result read_equality_expr(parser p, index offset, scope env) {
    result node = read_relational_expr(p, offset, env);
    Node r;
    
    result next = read_equality_expr(p, offset, env);
    if (next_token(p, offset, sym(==))) {
        r = binop(p, sym(==), node.v, next.v);
    } else if (next_token(p, offset, sym(!=))) {
        r = binop(p, sym(!=), node.v, next.v);
    } else {
        return node;
    }
    
    // ok - yes, we know this is a boolean. binop _should_ take a type? 
    // set(r, sym(type), pget(env, sym(type), sym(int)));
    return res(r, offset);
}

// this is precidence....i'd rather railroad - can we do that pure?
static result read_bitand_expr(parser p, index offset, scope env) {
    result node = read_equality_expr(p, offset, env);
    value v = node.v;
    while (next_token(p, offset, sym(&))){
        result next = read_equality_expr(p, offset, env);
        v = binop(p, sym(&), v, next.v);
    }
    return res(v, offset);
}

static result read_bitxor_expr(parser p, index offset, scope env) {
    result node = read_bitand_expr(p, offset, env);
    value n = node.v;
    while (next_token(p, offset, sym(^)))
        n = binop(p, sym(^), node.v, read_bitand_expr(p, offset, env).v);
    return res(n, offset);
}

static result read_bitor_expr(parser p, index offset, scope env) {
    result node = read_bitxor_expr(p, offset, env);
    value v = node.v;
    while (next_token(p, offset, sym(|))) {
        result v2 = read_bitxor_expr(p, offset, env);
        v = binop(p, sym(|), v, v2.v);
    }
    return res(v, offset);
}

static result read_logand_expr(parser p, index offset, scope env) {
    result node = read_bitor_expr(p, offset, env);
    value v;
    while (next_token(p, offset, sym(&&))) {
        result n = read_bitor_expr(p, offset, env);
        v = n.v;
        v = ast_binop(pget(env, sym(type), sym(int)), sym(&&), node.v, v);
    }
    return res(v, offset);
}

// maybe just do railyard after all
static result read_logor_expr(parser p, index offset, scope env) {
    result node = read_logand_expr(p, offset, env);
    value n = node.v;
    while (next_token(p, offset, sym(||))) {
        result le = read_logand_expr(p, offset, env);
        n = ast_binop(pget(env, sym(type), sym(int)), sym(||), n, le.v);
    }
    return res(n, offset);
}

static result read_conditional_expr(parser p, index offset,  scope env);

static Node ast_ternary(Type ty, Node cond, Node then, Node els)
{
    return timm("kind", sym(ternary), "cond", cond, "then", then, "else", els);
}

static result do_read_conditional_expr(parser p, index offset, scope env, Node cond) {
    result then = read_expr(p, offset, env);
    expect(p, offset, sym(:));
    result els = read_conditional_expr(p, offset, env);
    // [GNU] Omitting the middle operand is allowed.
    Type t = then.v ? pget(then.v, sym(type)): pget(cond, sym(type));
    Type u = pget(els.v, sym(ty));
    // C11 6.5.15p5: if both types are arithemtic type, the result
    // type is the result of the usual arithmetic conversions.
    // type unification is a sometimes-treat
    if (is_inttype(t) && is_inttype(u)) {
        Type r = usual_arith_conv( t, u);
        return res(ast_ternary(r, cond, (then.v ? wrap(r, then.v) : zero), wrap(r, els.v)), offset);
    }
    return res(ast_ternary(u, cond, then.v, els.v), offset);
}

static result read_conditional_expr(parser p, index offset, scope env) {
    result cond = read_logor_expr(p, offset, env);
    if (!next_token(p, offset, sym(?)))
        return cond;
    return do_read_conditional_expr(p, offset, env, cond.v);
}

static string get_compound_assign_op(tuple tok) {
    if (pget(tok, sym(kind)) != sym(keyword))
        return 0;
    return(pget(tok, sym(id)));
}


result read_assignment_expr(parser p, index offset, scope env) {
    result node = read_logor_expr(p, offset, env);
    tuple tok = token(p, offset);
    if (!tok) return node;
    if (is_keyword(tok, sym(?)))
        return do_read_conditional_expr(p, node.offset, env, node.v);
    string cop = get_compound_assign_op(tok);
    if (is_keyword(tok, sym(=)) || cop) {
        result value = read_assignment_expr(p, offset, env);
        Node right = cop ? binop(p, cop, node.v, value.v) : value.v;
        Type ty = pget(node.v, sym(type));
        if (is_inttype(ty) && pget(ty, sym(kind)) != pget(right, sym(ty), sym(kind)))
            right = ast_conv(ty, right);
        return res(ast_binop(ty, sym(=), node.v, right), offset);
    }
    return node;
}


// dont simplify this?
result read_subscript_expr(parser p, index offset, scope env, Node node) {
    tuple tok = token(p, offset);
    result sub = read_expr(p, offset, env);
    if (!sub.v) error(p, "subscript expected");
    expect(p, offset, stringify("]"));
    Node t = binop(env, sym(+), node, sub.v);
    return res(ast_unaryop(sym(deref), pget(t, sym(type), sym(ptr)), t), offset);
}


result read_expr(parser p, index offset, scope env) {
    result node = read_assignment_expr(p, offset, env);
    value r;
    while (next_token(p, offset, stringify(","))){
        result expr = read_assignment_expr(p, offset, env);
        r = ast_binop(pget(expr.v, sym(type)), stringify(","), node.v, expr.v);
    }
    return res(r, offset);
}


static result read_unary_incdec(parser p, index offset, scope env, string op) {
    result operand = read_unary_expr(p, offset, env);
    return res(ast_unaryop(op, pget(operand.v, sym(type)), operand.v), operand.offset);
}

static result read_label_addr(parser p, index offset, scope env, tuple tok) {
    // [GNU] Labels as values. You can get the address of the a label
    // with unary "&&" operator followed by a label name.
    tuple tok2 = token(p, offset);
    if (pget(tok2, sym(kind)) != sym(identifier))
        error(p, "label name expected after &&, but got %s", tok2);
    // type void
    Node r = timm("kind", sym(label_addr), "type",
                  make_ptr_type(pget(env, sym(type), sym(void))),
                  "name", pget(tok2, sym(value)));
    
    // push(pget(env, sym(gotos)), r);
    return res(r, offset);
}

static result read_unary_addr(parser p, index offset, scope env) {
    result operand = read_cast_expr(p, offset, env);
    if (pget(operand.v, sym(kind)) == sym(funcdesg))
        return operand;
    return res(ast_unaryop(sym(addr), make_ptr_type(pget(operand.v, sym(type))), operand.v), offset);
}

static result read_unary_deref(parser p, index offset, scope env, tuple tok) {
    result operand = read_cast_expr(p, offset, env);
    Type ot = pget(operand.v, sym(type));
    if (pget(ot,sym(kind)) != sym(ptr))
        error(p, "pointer type expected, but got %s", node2s(operand));
    if (pget(ot, sym(ptr), sym(kind)) == sym(func))
        return operand;
    return res(ast_unaryop(sym(deref), pget(ot, sym(ptr)), operand.v), offset);
}

static result read_unary_minus(parser p, index offset, scope env) {
    result expr = read_cast_expr(p, offset, env);
    return res(binop(p, sym(-), ast_int_literal(pget(expr.v, sym(type), 0), expr.v), expr.v), offset);
}

static result read_unary_bitnot(parser p, index offset, scope env, tuple tok) {
    result expr = read_cast_expr(p, offset, env);
    Type et = pget(expr.v, sym(type));
    if (!is_inttype(et))
        error(p, "invalid use of ~: %s", node2s(expr));
    return res(ast_unaryop(sym(~), et, expr.v), expr.offset);
}

static result read_unary_lognot(parser p, index offset, scope env) {
    result operand = read_cast_expr(p, offset, env);
    return res(ast_unaryop(sym(!), pget(env, sym(type), sym(int)), operand.v),
               operand.offset);
}



static result read_stmt_expr(parser p, index offset, scope env) {
    result r = read_compound_stmt(p, offset, env);
    expect(p, offset, stringify(")"));
    Type rtype = pget(env, sym(type), sym(void));

    value st;
    if ((st = pget(r.v, "statements"))) {
        Node lastexpr = vector_peek(st);
        value v;
        if ((v = pget(lastexpr, sym(type))))
            rtype = v;
    }
    return res(allocate_scope(r, sym(type), rtype), offset);
}

// fold 
static Node ast_funcdesg(Type ty, string fname)
{
    return timm("kind", sym(funcdesg), "tupe", ty, "fname", fname);
}

static result read_var_or_func(parser p, index offset, scope env, buffer name) {
    Node v = pget(env, name);
    if (!v) {
        tuple tok = token(p, offset);
        if (!is_keyword(tok, stringify("(")))
            error(p, "undefined variable: %s", name);
        Type ty = make_func_type(pget(env, sym(type), sym(int)), 0);
        // warnt(tok, "assume returning int: %s()", name);
        return res(ast_funcdesg(ty, name), offset);
    }
    // so..funcdesg is really just a variable of function type?
    if (pget(v, sym(type), sym(kind)) == sym(func))
        return res(ast_funcdesg(pget(v, sym(type)), name), offset);
    return res(v, offset);
}

static result read_primary_expr(parser p, index offset, scope env) {
    tuple tok = token(p, offset);
    // if (!tok) return zero;
    value k = pget(tok, sym(kind));
    value v = pget(tok, sym(value));
    if (is_keyword(tok, stringify("("))) {
        if (next_token(p, offset, stringify("[")))
            return read_stmt_expr(p, offset, env);
        result r = read_expr(p, offset, env);
        expect(p, offset, stringify(")"));
        return r;
    }
    if (k == sym(keyword)) return res(k, offset);
    if (k == sym(identifier)) return read_var_or_func(p, offset, env, v);
    //  if (tok->kind == sym(number))
    //    return read_int(p, tok);
    if (k == sym(char))  return res(ast_int_literal(pget(env, sym(type), sym(char)), v), offset);
    if (k == sym(string)) return res(ast_string(env, v), offset);
    error(p, "internal error: unknown token kind: %d", k);
    return res(zero, offset);
}



static result read_sizeof_operand(parser p, index offset, scope env) {
    tuple tok = get(p->tokens, (value)offset);
    if (is_keyword(tok, stringify("(")) && is_type(p, token(p, offset))) {
        result r = read_cast_type(p, offset, env);

        expect(p, offset, stringify(")"));
        return r;
    }    
    Type ty = pget(read_unary_expr(p, offset, env).v, sym(type));
    string tyk = pget(ty, sym(kind));
    // Sizeof on void or function type is GNU extension
    value size = (tyk == sym(void) || tyk == sym(func)) ?
        value_from_u64(1) : pget(ty, sym(size));
    return res(ast_int_literal(pget(env, sym(type), sym(ulong)), size), 0);
}

// do we really .. want .. alignof?
static result read_alignof_operand(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    result ty = read_cast_type(p, offset, env);
    expect(p, offset, stringify(")"));
    return res(ast_int_literal(pget(env, sym(types), sym(ulong)),  pget(env, ty.v, 0)), 0);
}

static result read_unary_expr(parser p, index offset, scope env) {
    tuple tok = token(p, offset);
    if (pget(tok, sym(kind)) == sym(keyword)) {
        value id = pget(tok, sym(id));
        if (id == sym(sizeof)) return read_sizeof_operand(p, offset, env);
        if (id == sym(alignof)) return read_alignof_operand(p, offset, env);
        if (id == sym(pre_inc)) return read_unary_incdec(p, offset, env, sym(pre_inc));
        if (id == sym(pre_dec)) return read_unary_incdec(p, offset, env, sym(pre_dec));
        // computed goto?
        if (id == sym(&&)) return read_label_addr(p, offset, env, tok);
        if (id == sym(&)) return read_unary_addr(p, offset, env);
        if (id == sym(*)) return read_unary_deref(p, offset, env, tok);
        if (id == sym(+)) return read_cast_expr(p, offset, env);
        if (id == sym(-)) return read_unary_minus(p, offset, env);
        if (id == sym(~)) return read_unary_bitnot(p,offset,  env, tok);
        if (id == sym(!)) return read_unary_lognot(p, offset, env);
    }
    result r = read_primary_expr(p, offset, env);
    return read_postfix_expr_tail(p, r.offset, env, r.v);
}

