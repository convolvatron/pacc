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
        read_decl(p, offset, env, list);
    } else {
        Node stmt = read_stmt(p, offset, env);
        //   if (stmt)
        //  push(list, stmt);
    }
}

static Node ast_compound_stmt(vector stmts)
{
    return timm("kind", sym(compound_stmt), "statments", stmts);
}

static Node read_opt_decl_or_stmt(parser p, index offset, scope env) {
    if (next_token(p, offset, stringify(";")))
        return zero;
    vector list = 0;
    read_decl_or_stmt(p, offset, env, list);
    return ast_compound_stmt(list);
}

static Node ast_if(Node cond, Node then, Node els) {
    return timm("kind", sym(if), "cond", cond, "then", then, "else", els);
}


static Node read_if_stmt(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    Node cond = read_expr(p, offset, env);
    expect(p, offset, stringify(")"));
    Node then = read_stmt(p, offset, env);
    if (!next_token(p, offset, sym(else)))
        return ast_if(cond, then, zero);
    Node els = read_stmt(p, offset, env);
    return ast_if(cond, then, els);
}

Node read_compound_stmt(parser p, index offset, scope env)
{
    // push_scope(p);
    vector list = 0;
    for (;;) {
        if (next_token(p, offset, stringify("}")))
            break;
        read_decl_or_stmt(p, offset, env, list);
    }
    return ast_compound_stmt(list);
}



static Node read_do_stmt(parser p, index offset, scope env)
{
    buffer beg = make_label();
    buffer end = make_label();
    Node body = read_stmt(p, offset, env);
    
    tuple tok = token(p, offset);
    if (!is_keyword(tok, sym(while)))
        error(p, "'while' is expected, but got %s", tok);
    expect(p, offset, stringify("("));
    Node cond = read_expr(p, offset, env);
    expect(p, offset++, stringify(")"));
    expect(p, offset++, stringify(";"));

    return  ast_compound_stmt(timm("begin", ast_dest(beg),
                                   "if", ast_if(cond, ast_jump(beg), zero),
                                   "body", body,
                                   "end", ast_dest(end)));
}

static Node make_switch_jump(parser p, Node var, tuple c) {
    Node cond;
    Type int_type = pget(p->global, sym(type), sym(int)); // ?
    if (pget(c, sym(beg)) == pget(c, sym(end))) {
        Type type_int = pget(p->global, sym(type), sym(int));
        cond = ast_binop(p, type_int, sym(=), var, ast_int_literal(p, int_type,
                                                                   pget(c, sym(begin))));
        //                                                               value_from_u64(c->beg)));
    } else {
        // [GNU] case i ... j is compiled to if (i <= cond && cond <= j) goto <label>.
        // fix lexical pointers
        Node x = ast_binop(p, int_type, sym(>=), ast_int_literal(p, int_type, pget(c, sym(beg))), var);
        Node y = ast_binop(p, int_type, sym(<=), var, ast_int_literal(p, int_type, pget(c, sym(end))));
        cond = ast_binop(p, int_type, sym(logand), x, y);
    }
    // not really
    return ast_if(cond, ast_jump(pget(c, sym(name))), zero);
}

static Node read_switch_stmt(parser p, index offset, scope env)
{
    expect(p, offset, stringify("("));
    Node expr = conv(p, read_expr(p, offset, env));
    expect(p, offset, stringify(")"));

    buffer end = make_label();
    // push_scope(p);
    Node body = read_stmt(p, offset, env);

        
    Node var = ast_var(env, pget(expr, sym(type)), make_tempname());
    // immutable?
    //foreach (i, v, pget(env, sym(cases)))
    //     push(v, make_switch_jump(p, var, v));
    
    value d = pget(env, sym(defaultcase));

    value v = timm("body", body,
                   "thing", ast_jump(d ?d : end),
                   "cond", ast_binop(p, pget(expr, sym(type)), sym(=), var, expr),
                   "end", ast_dest(end));
        
    return ast_compound_stmt(v);
}

static Node read_label_tail(parser p, index offset, scope env, Node label) {
    Node stmt = read_stmt(p, offset, env);
    vector v = 0;
    // push(v, label);
    //    if (stmt)
    //        push(v, stmt);
    return ast_compound_stmt(v);
}

// recurse and larvate
static Node read_case_label(parser p, index offset, scope env, tuple tok) {
    vector cases = pget(env, sym(cases));
    if (!cases) error(p, "stray case label");
    buffer label = make_label();
    int beg = read_intexpr(p, offset);
    if (next_token(p, offset, sym(...))) {
        int end = read_intexpr(p, offset);
        expect(p, offset, stringify(":"));
        if (beg > end)
            error(p, "case region is not in correct order: %d ... %d", beg, end);
        // push(cases, make_case(beg, end, label));
    } else {
        expect(p, offset, stringify(":"));
        // push(cases, make_case(beg, beg, label));
    }
    // inline - this seems..broken anyways - do it on insert!
    //   check_case_duplicates(cases);
    return read_label_tail(p, offset, env, ast_dest(label));
}

static Node read_default_label(parser p, index offset, scope env, tuple tok) {
    expect(p, offset, stringify(":"));
    if (pget(p->global, sym(defaultcase)))
        error(p, "duplicate default");
    value lab = make_label();
    return read_label_tail(p,
                           offset, 
                           allocate_scope(env, sym(defaultcase), lab), // lifetime
                           ast_dest(lab));
}

static Node read_break_stmt(parser p, index offset, scope env, tuple tok) {
    expect(p, offset, stringify(";"));
    value b;
    if (!(b =pget(env, sym(targets), sym(break))))
        error(p, "stray break statement");
    return ast_jump(b);
}

static Node read_continue_stmt(parser p, index offset, scope env, tuple tok) {
    expect(p, offset, stringify(";"));
    value lc;
    if (!(lc =pget(env, sym(targets), sym(continue))))
        error(p, "stray continue statement");
    return ast_jump(lc);
}


static Node ast_return(Node retval)
{
    return timm("kind", sym(return), "retval", retval);
}

static Node read_return_stmt(parser p, index offset, scope env) {
    Node retval = read_expr(p, offset, env);
    expect(p, offset, stringify(";"));
    if (retval)
        return ast_return(ast_conv(pget(env, sym(__return_type)), retval));
    return ast_return(zero);
}

static Node read_goto_stmt(parser p, index offset, scope env) {
    if (next_token(p, offset, sym(*))) {
        // [GNU] computed goto. "goto *p" jumps to the address pointed by p.

        Node expr = read_cast_expr(p, offset, env);
        if (pget(expr, sym(type), sym(kind)) != sym(ptr))
            error(p, "pointer expected for computed goto, but got %s", node2s(expr));
        return ast_computed_goto(expr);
    }
    tuple tok = token(p, offset);
    if (!tok || (pget(tok, sym(kind)) != sym(identifier)))
        error(p, "identifier expected, but got", tok);
    expect(p, offset, stringify(";"));
    Node r = ast_goto(pget(tok, sym(value)));
    // why...am I keep track of the gotos? for fixup? - yes
    // push(pget(p->global, sym(gotos)), r);
    return r;
}

static Node read_label(parser p, index offset, scope env, tuple tok)
{
    buffer label = pget(tok, sym(sval));
    if (pget(env, sym(labels), label))
        error(p, "duplicate label: %s", tok);
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

static Node read_for_stmt(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    // jumps go to nodes
    buffer beg = make_label();
    buffer mid = make_label();
    buffer end = make_label();
    // push_scope(p);
    Node init = read_opt_decl_or_stmt(p, offset, env);
    Node cond = read_expr(p, offset, env);
    expect(p, offset, stringify(";"));
    Node step = read_expr(p, offset, env);
    expect(p, offset, stringify(")"));
    Node body = read_stmt(p, offset, env);

    return ast_compound_stmt(timm("init", init,
                                  "cond", cond?ast_if(cond, zero, ast_jump(end)):0,
                                  "step", step,
                                  "begin", ast_jump(beg),
                                  "end", ast_dest(end)));
}


#define allocate_vector(...) true

static Node read_while_stmt(parser p, index offset, scope env) {
    expect(p, offset, stringify("("));
    Node cond = read_expr(p, offset, env);
    expect(p, offset, stringify(")"));

    buffer beg = make_label();
    buffer end = make_label();
    // push_scope(p);
    //   SET_JUMP_LABELS(beg, end);
    Node body = read_stmt(p, offset, env);


    // why not timm here?
    return ast_compound_stmt(timm(// "end", ast_dest(begin), // ? 
                                  "cond", ast_if(cond, body, ast_jump(end)),
                                  "begin", ast_jump(beg),
                                  "end", ast_dest(end)));

}


// maybe index is in the semantic value?
Node read_stmt(parser p, index offset, scope env) {
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

    Node r = read_expr(p, offset, env);
    expect(p, offset, stringify(";"));
    return r;
}
