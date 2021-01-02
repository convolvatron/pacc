#include <pacc.h>

// should be a node?
static Node ast_goto(buffer label)
{
    return timm("kind", sym(goto), "label", label);
}

static Node ast_computed_goto(Node expr) {
    return timm("kind", sym(computed_goto), "operand", expr);
}

static Node ast_jump(buffer label)
{
    return timm("kind", sym(goto), "label", label);
}

static Node ast_dest(buffer label) {
    return timm("kind", sym(label), "name", label);
}

string make_tempname() {
    static int c = 0;
    u8 staging[20];
    return allocate_utf8(staging, sprintf((char *)staging, ".T%d", c++)); // xx - libc
}


static void read_decl_or_stmt(parser p, index offset, scope env, vector list) {
    tuple tok = token(p, offset);
    // mark_location();
    if (is_type(p, tok)) {
        // off
        read_declaration(p, offset, env);
    } else {
        result stmt = read_statement(p, offset, env);
        //   if (stmt)
        //  push(list, stmt);
    }
}

static Node ast_compound_stmt(vector stmts)
{
    return timm("kind", sym(compound_stmt), "statments", stmts);
}

// better general optionalizer
static result read_opt_decl_or_stmt(parser p, index offset, scope env) {
    if (next_token(p, offset, stringify(";")))
        return res(zero, offset);
    vector list = 0;
    read_decl_or_stmt(p, offset, env, list);
    return res(ast_compound_stmt(list), offset);
}

static Node ast_if(Node cond, Node then, Node els) {
    return timm("kind", sym(if), "cond", cond, "then", then, "else", els);
}


static result read_if_stmt(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    result cond = read_expression(p, offset, env);
    expect(p, offset, stringify(")"));
    result then = read_statement(p, offset, env);
    if (!next_token(p, offset, sym(else)))
        return res(ast_if(cond.v, then.v, zero), then.offset);
    result els = read_statement(p, offset, env);
    return res(ast_if(cond.v, then.v, els.v), els.offset);
}

result read_compound_stmt(parser p, index offset, scope env)
{
    // push_scope(p);
    vector list = 0;
    for (;;) {
        if (next_token(p, offset, stringify("}")))
            break;
        read_decl_or_stmt(p, offset, env, list);
    }
    return res(ast_compound_stmt(list), offset);
}



static result read_do_stmt(parser p, index offset, scope env)
{
    buffer beg = make_label();
    buffer end = make_label();
    result body = read_statement(p, offset, env);
    
    tuple tok = token(p, offset);
    if (!is_keyword(tok, sym(while)))
        error("'while' is expected, but got %s", tok);
    expect(p, offset, stringify("("));
    result cond = read_expression(p, offset, env);
    expect(p, cond.offset+1, stringify(")"));
    expect(p, cond.offset+2, stringify(";"));
    
    return res(ast_compound_stmt(timm("begin", ast_dest(beg),
                                      "if", ast_if(cond.v, ast_jump(beg), zero),
                                      "body", body,
                                      "end", ast_dest(end))), cond.offset+3);
}

#if 0
// wtf is going on here...is this per case? ranges?
static Node make_switch_jump(scope env, Node var, tuple c) {
    Node cond;
    Type int_type = pget(env, sym(type), sym(int));
    if (pget(c, sym(beg)) == pget(c, sym(end))) {
        Type type_int = pget(env, sym(type), sym(int));
        cond = ast_binop(type_int, sym(=), var, ast_int_literal(int_type,
                                                                     pget(c, sym(begin))));
        //                                                               value_from_u64(c->beg)));
    } else {
        // [GNU] case i ... j is compiled to if (i <= cond && cond <= j) goto <label>.
        // fix lexical pointers
        Node x = ast_binop(int_type, sym(>=), ast_int_literal(int_type, pget(c, sym(beg))), var);
        Node y = ast_binop(int_type, sym(<=), var, ast_int_literal(int_type, pget(c, sym(end))));
        cond = ast_binop(int_type, sym(logand), x, y);
    }
    // not really
    return ast_if(cond, ast_jump(pget(c, sym(name))), zero);
}
#endif

static result read_switch_stmt(parser p, index offset, scope env)
{
    expect(p, offset, stringify("("));
    result expr = read_expression(p, offset, env);
    expect(p, expr.offset, stringify(")"));

    buffer end = make_label();
    // push_scope(p);
    result body = read_statement(p, offset, env);

        
    Node var = ast_var(env, pget(expr.v, sym(type)), make_tempname());
    // immutable?
    //foreach (i, v, pget(env, sym(cases)))
    //     push(v, make_switch_jump(p, var, v));
    
    value d = pget(env, sym(defaultcase));

    value v = timm("body", body,
                   "thing", ast_jump(d ?d : end),
                   "cond", ast_binop(pget(expr.v, sym(type)), sym(=), var, expr.v),
                   "end", ast_dest(end));
        
    return res(ast_compound_stmt(v), offset);
}

static result read_label_tail(parser p, index offset, scope env, Node label) {
    result stmt = read_statement(p, offset, env);
    vector v = 0;
    // push(v, label);
    //    if (stmt)
    //        push(v, stmt);
    return res(ast_compound_stmt(v), stmt.offset);
}

// recurse and larvate
static result read_case_label(parser p, index offset, scope env, tuple tok) {
    vector cases = pget(env, sym(cases));
    if (!cases) error("stray case label");
    buffer label = make_label();
    int beg = read_intexpr(p, offset);
    if (next_token(p, offset, sym(...))) {
        int end = read_intexpr(p, offset);
        expect(p, offset, stringify(":"));
        if (beg > end)
            error("case region is not in correct order: %d ... %d", beg, end);
        // push(cases, make_case(beg, end, label));
    } else {
        expect(p, offset, stringify(":"));
        // push(cases, make_case(beg, beg, label));
    }
    // inline - this seems..broken anyways - do it on insert!
    //   check_case_duplicates(cases);
    return read_label_tail(p, offset, env, ast_dest(label));
}

static result read_default_label(parser p, index offset, scope env, tuple tok) {
    expect(p, offset, stringify(":"));
    //    if (pget(env, sym(defaultcase)))
    //        error("duplicate default");
    value lab = make_label();
    return read_label_tail(p,
                           offset, 
                           allocate_scope(env, sym(defaultcase), lab), // lifetime
                           ast_dest(lab));
}

static result read_break_stmt(parser p, index offset, scope env, tuple tok) {
    expect(p, offset, stringify(";"));
    value b;
    if (!(b =pget(env, sym(targets), sym(break))))
        error("stray break statement");
    return res(ast_jump(b), offset);
}

static result read_continue_stmt(parser p, index offset, scope env, tuple tok) {
    expect(p, offset, stringify(";"));
    value lc;
    if (!(lc =pget(env, sym(targets), sym(continue))))
        error("stray continue statement");
    return res(ast_jump(lc), offset);
}


static Node ast_return(Node retval)
{
    return timm("kind", sym(return), "retval", retval);
}

static result read_return_stmt(parser p, index offset, scope env) {
    result retval = read_expression(p, offset, env);
    expect(p, offset, stringify(";")); // cant we genernicize this?
    // ternary
    if (retval.v)
        return res(ast_return(ast_conv(pget(env, sym(__return_type)), retval.v)), offset);
    return res(ast_return(zero), offset);
}

static result read_goto_stmt(parser p, index offset, scope env) {
    if (next_token(p, offset, sym(*))) {
        // [GNU] computed goto. "goto *p" jumps to the address pointed by p.

        result expr = read_cast_expression(p, offset, env);
        if (pget(expr.v, sym(type), sym(kind)) != sym(ptr))
            error("pointer expected for computed goto, but got %s", node2s(expr));
        return res(ast_computed_goto(expr.v), expr.offset);
    }
    tuple tok = token(p, offset);
    if (!tok || (pget(tok, sym(kind)) != sym(identifier)))
        error("identifier expected, but got", tok);
    expect(p, offset, stringify(";"));
    Node r = ast_goto(pget(tok, sym(value)));
    // why...am I keep track of the gotos? for fixup? - yes
    // push(pget(p->global, sym(gotos)), r);
    return res(r, offset);
}

static result read_label(parser p, index offset, scope env, tuple tok)
{
    buffer label = pget(tok, sym(sval));
    if (pget(env, sym(labels), label))
        error("duplicate label: %s", tok);
    Node r = timm("kind", sym(label), "name", label);
    //    xxx - update - set(pget(p->global, sym(labels)), sym(label), r);
    return read_label_tail(p, offset, env, r);
}


// maybe this should just be for generation? since we are allowing cycles?
string make_label() {
    static int c = 0;
    u8 staging[20];
    return allocate_utf8(staging, sprintf((char *)staging, ".L%d", c++)); // xx - libc
}

static tuple make_case(int beg, int end, buffer label) {
    return timm("begin", beg, "end", end, "label", label);
}

static result read_for_stmt(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    // jumps go to nodes
    buffer beg = make_label();
    buffer mid = make_label();
    buffer end = make_label();
    // push_scope(p);
    result init = read_opt_decl_or_stmt(p, offset, env);
    result cond = read_expression(p, offset, env);
    expect(p, offset, stringify(";"));
    result step = read_expression(p, offset, env);
    expect(p, offset, stringify(")"));
    result body = read_statement(p, offset, env);

    // ast jump isn't really a thing
    return res(ast_compound_stmt(timm("init", init,
                                      "cond", cond.v?ast_if(cond.v, zero, ast_jump(end)):0,
                                      "step", step,
                                      "begin", ast_jump(beg),
                                      "end", ast_dest(end))), body.offset);
}


#define allocate_vector(...) true

static result read_while_stmt(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    result cond = read_expression(p, offset, env);
    expect(p, offset, stringify(")"));

    buffer beg = make_label();
    buffer end = make_label();
    // push_scope(p);
    //   SET_JUMP_LABELS(beg, end);
    result body = read_statement(p, offset, env);


    // why not timm here?
    return res(ast_compound_stmt(timm(// "end", ast_dest(begin), // ? 
                                       "cond", ast_if(cond.v, body.v, ast_jump(end)),
                                       "begin", ast_jump(beg),
                                       "end", ast_dest(end))), offset);

}


// maybe index is in the semantic value?
result read_statement(parser p, index offset, scope env) {
    tuple tok = token(p, offset);
    value id = pget(tok, sym(id));
    value k = pget(tok, sym(kind));
    if (k == sym(keyword)) {
        if (id == stringify("{")) read_compound_stmt(p, offset, env);
        if (id == sym(if)) read_if_stmt(p, offset, env);
        if (id == sym(for)) read_for_stmt(p, offset, env);
        if (id == sym(while))  return read_while_stmt(p, offset, env);
        if (id == sym(do))    return read_do_stmt(p, offset, env);
        if (id == sym(return))  return read_return_stmt(p, offset, env);
        if (id == sym(switch))  return read_switch_stmt(p, offset, env);
        if (id == sym(case))   return read_case_label(p, offset, env, tok);
        if (id == sym(default)) return read_default_label(p, offset, env,  tok);
        if (id == sym(break))  return read_break_stmt(p, offset, env, tok);
        if (id == sym(continue)) return read_continue_stmt(p, offset, env, tok);
        if (id == sym(goto))   return read_goto_stmt(p, offset, env);
    }
    if ((k == sym(identifier)) && next_token(p, offset, stringify(":")))
        return read_label(p, offset, env, tok);

    result r = read_expression(p, offset, env);
    expect(p, offset, stringify(";"));
    return r;
}
