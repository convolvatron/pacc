static Type read_enum_def(parser p, scope env) {
    buffer tag = zero;
    tuple tok = token(p);

    // Enum is handled as a synonym for int. We only check if the enum
    // is declared.
    if (get(tok, sym(kind)) == sym(identifier)) {
        tag = get(tok, sym(value));
        tok = token(p);
    }
    if (tag) {
        Type ty = pget(env, sym(tags), tag);
        if (ty && get(ty, sym(kind)) != sym(enum))
            error(p, "declarations of %s does not match", tag);
    }
    if (!is_keyword(tok, stringify("{"))) {
        if (!tag || !pget(env, sym(tags), tag))
            error(p, "enum tag %s is not defined", tag);
        return pget(p->global, sym(type), sym(int));
    }
    if (tag)
        tag = allocate_scope(get(p, sym(tags)),
                             tag,
                             get(p->global, sym(type), sym(enum)));

    int val = 0;
    for (;;) {
        tok = token(p);
        if (is_keyword(tok, stringify("}")))
            break;
        if (get(tok, sym(kind)) != sym(identifier))
            error(p, "identifier expected, but got %s", tok);
        buffer name = get(tok, sym(value));

        if (next_token(p, sym(=)))
            val = read_intexpr(p);
        Node constval = ast_inttype(p, pget(p->global, sym(type), sym(int)),
                                    value_from_u64(val));
        // increment val
        // adding to the namespace
        //    set(get(p->global, sym(tags)), name, constval);

        if (next_token(p, sym(strinigy(","))))
            continue;
        if (next_token(p, stringify("}")))
            break;
        error(p, "',' or '}' expected, but got", peek())
    }
    return pget(p->global, sym(type), sym(int));
}


#if 0
// C11 5.1.1.2p6 Adjacent string literal tokens are concatenated.
static void concatenate_string(parser p, tuple tok) {
    buffer b = allocate_buffer(p->h, 10);
    //push_buffer(b, tok->sval);
    while (token(p)->kind == sym(string)) {
        tuple tok2 = token(p);
        //push_buffer(b, tok2->sval);
    }
    tok->sval = b;
}
#endif
