#include <pacc.h>

static Node ast_decl(Node var, vector init) {
    return timm("kind", sym(decl),
                "declinit", init,
                "declvar", var);
}

static Node ast_init(Node val, Type totype) {
    return timm("kind", sym(init),
                "initval", val,
                "totype", totype);
}


static result read_initializer_elem(parser p,
                                    scope env,
                                    Type ty,
                                    index offset);

// we can make a groupie - read_util (parser, reader)
// syntax only please
static result read_struct_initializer(parser p,
                                      index offset,
                                      scope env) {

    boolean has_brace = toboolean(next_token(p, offset, stringify("{")));
    int i = 0;
    
    tuple tok = token(p, offset);
    if (is_keyword(tok, stringify("}"))) return res(zero, offset + 1);
    
    // there was something about nested bracket initializers
    if (is_keyword(tok, sym(.))) {
        tok = token(p, ++offset);
        if (!tok || pget(tok, sym(kind)) != sym(identifier))
            error(p, "malformed desginated initializer: %s", tok);
    } 
    read_initializer_elem(p, env, offset);
    next_token(p,  ++offset, stringify(","));
    return res(0, offset);
}



int read_intexpr(parser p, index offset) {
    // xxx - we were doing static evaluation here...pass through
    // take that back, this gets used for array bounds and such
    return 0;
}

static result read_array_initializer(parser p,
                                    scope env,
                                    vector inits,
                                    index offset) {
    
    boolean has_brace = toboolean(next_token(p, offset, stringify("{")));
    boolean flexible = toboolean(pget(ty, sym(len)) <= 0);
    int i;
    
    for (i = 0; flexible || i < u64_from_value(pget(ty, sym(len))); i++) {
        tuple tok = token(p, offset);
        // wrapper functions?
        if (is_keyword(tok, stringify("}"))) {
            if (!has_brace) offset--;
            return offset;
        }
        
        if ((is_keyword(tok, sym(.)) || is_keyword(tok, stringify("{"))) && !has_brace) {
            offset--;
            return offset;
        }
        
        if (is_keyword(tok, stringify("{"))) {
            int idx = read_intexpr(p, offset);
            //      if (idx < 0 || (!flexible && ty->len <= idx))
            //        error(p, "array designator exceeds array bounds: %d", idx);
            i = idx;
            expect(p, offset, stringify("}"));
        }
        read_initializer_elem(p, env, inits, offset);
        next_token(p, offset, stringify(","));
    }
    if (has_brace) expect(p, offset, stringify("}"));
    // finish:
    // is this is a default int thing?
    //    if (!pget(ty, sym(len))) {
    //        set(ty, sym(len), value_from_u64(i));
    //        set(ty, sym(size), value_from_u64(u64_from_value(elemsize) * i));
    //    }
    return offset;
}

static result read_initializer_list(parser p,
                                   index offset,
                                   scope env,
                                    Type ty)
{
    tuple tok = token(p, offset);
    value k = pget(tok, sym(kind));
    value v = pget(tok, sym(value));

    if (is_string(ty)) {
        if (pget(tok, sym(kind)) == sym(string)) {
            //            assign_string(p, inits, ty, v);
            return res(zero, offset);
        }
        if (is_keyword(tok, stringify("{")) && (pget(env, sym(kind)) == sym(string))) {
            tok = token(p, offset);
            //assign_string(p, inits, ty, v);
            expect(p, offset + 1, stringify("}"));
            return res(v, offset + 1);
        }
    }
    offset--;
    string tk = pget(ty, sym(kind));
    if (tk == sym(array)) {
        read_array_initializer(p, env, offset);
    } else if (tk == sym(struct)) {
        read_struct_initializer(p, offset, env, ty);
    } else {
        Type arraytype = make_array_type(ty, 1);
        read_array_initializer(p, env, offset);
    }
    return offset;
}


static result read_rectype_def(parser p, index offset, scope env, string kind);

result read_decl_spec(parser p, index offset, scope env, string rsclass) {
    value tok = token(p, offset);
    value k = pget(tok, sym(kind));
    
    if (k == sym(identifier)){
        return result{pget(s, sym(types), name), offset+1};        
        result def = pget(env, sym("type"), pget(tok, sym(value)));
        if (def) return res(def, zero);
    }
    
    value id = pget(tok, sym(value));
    
    if ((id == sym(struct)) || (id == sym(union)))
        return read_rectype_def(p, offset, env, id); 

#if 0
    if (id == sym(enum)) return read_enum_def(p, env);
    

    if (id == sym(alignas)) {
        value nval = read_alignas(p);
        // C11 6.7.5p6: alignas(0) should have no effect.
        if (nval != 0) {
            if (align == -1 || val < align)
                align = nval;
        }
    }

    if (id == sym(typeof)) 
        return read_typeof(p, env);
#endif
    
    error(p, "type mismatch", tok);
    return res(zero, 0);
}

static result read_rectype_def(parser p, index offset, scope env, string kind) {
    value tag = zero;
    tuple tok = token(p, offset);
    if (get(tok, sym(kind)) == sym(identifier)) {
        tag = get(tok, sym(value));
    }
    value r;
    if (tag) { // what.. is a tag?
        r = pget(env, sym(tags), tag);
        if (r && (pget(r, sym(kind)) == sym(enum) || pget(r, sym(kind)) != kind))
            error(p, "declarations of %s does not match", tag);
        if (!r) {
            r = timm("kind", kind,
                     "tags", r); // ?
        }
    } else {
        r = timm("kind", kind);
    }

    tuple r = 0;
    result basetype = read_decl_spec(p, offset, env, zero);
    // seems odd
    //        if (pget(basetype, sym(kind)) == sym(struct) && next_token(p, sym(;)))
    // push(r, timm(type, basetype));
    //            continue;
    //        }
    
    buffer name = zero;
    result fieldtype = read_declarator(p, offset, env, &name, basetype.v, zero);
    fieldtype = allocate_scope(fieldtype);
    
    //      if (next_token(p, stringify(":")))
    // just change to readintexpr - there were some checks
    //        set(fieldtype, sym(bitsize), read_bitsize(p, name, fieldtype));

    // clearly we deconstructed something a little too far here
    //    if (next_token(p, offset, stringify(","))) continue;

    
    // seems like expect could be broadened
    value v;
    if (is_keyword(v = token(p, offset), stringify("}"))) {
        error(p, "missing ';' at the end of field list", v);
    } else
        expect(p, offset, stringify(";"));
    
    timm("name", name, "type", fieldtype);

    expect(p, offset, stringify("}"));
    // umm..       (!next_token(p, offset, stringify("{")))?zero:read_rectype_fields_sub(p, offset, env));    
    return res(timm("fields", zero), offset);

}

Node read_unary_expr(parser p, scope env);

#define vector_length(_x) 1

static result read_initializer_elem(parser p,
                                    scope env,
                                    Type ty,
                                    index offset) {
    next_token(p, offset, sym(=));
    if (pget(ty, sym(kind)) == sym(array) || pget(ty, sym(kind)) == sym(struct)) {
        read_initializer_list(p, offset, env, ty);
    } else if (next_token(p, offset, stringify("{"))) {
        result e = read_initializer_elem(p, env, ty, offset);
        expect(p, offset, stringify("}"));
    } else {
        return conv(p, read_assignment_expr(p, offset, env));
        // ensure_assignable(ty, pget(expr, sym(type)));
        // push(inits, ast_init(expr, ty));
    }
}

// generic optional
static result read_func_param(parser p, index offset, scope env, buffer *name) {
    string sclass = 0;
    result basety = read_decl_spec(p, offset, env, sclass);
    
    //    if (!basety.v) {
    //        // defer
    //        if (!optional) {
    //            error(p, "type expected, but got", peek());
    //        }
    //    }

    result ty = read_declarator(p, basety.offset, env, name, basety.v);

    // C11 6.7.6.3p7: Array of T is adjusted to pointer to T
    // in a function parameter list.
    if (pget(ty.v, sym(kind)) == sym(array))
        return res(make_ptr_type(pget(ty.v, sym(ptr))), offset);
    // C11 6.7.6.3p8: Function is adjusted to pointer to function
    // in a function parameter list.
    if (pget(ty.v, sym(kind)) == sym(func))
        return res(make_ptr_type(ty.v), offset);
    return ty;
}


// Reads an ANSI-style prototyped function parameter list...this is the same, but
// we're deferring many checks
result read_declarator_params(parser p, index offset, scope env, vector types)  {
    int length;

    for (;;) {
        tuple tok = token(p, offset);
        if (next_token(p, offset, sym(...))) {
            if (!types) error(p, "at least one parameter is required before \"...\"");
            expect(p, offset, stringify(")"));
            goto construct;
        }
        buffer name;
        result ty = read_func_param(p, offset, env, &name); 

        tok = token(p, offset);
        if (is_keyword(tok, stringify(")")))
            goto construct;

        if (!is_keyword(tok, stringify(",")))
            error(p, "stringify(",") expected, but got %s", tok);
    }
 construct:
    return res(zero, offset); // build vector
}

static Type read_func_param_list(parser p, index offset, scope env, Type rettype) {
    // C11 6.7.6.3p10: A parameter list with just "void" specifies that
    // the function has no parameters.
    tuple tok = token(p, offset);
    if (is_keyword(tok, sym(void)) && next_token(p, offset, stringify(")")))
        return make_func_type(rettype, zero);

    // C11 6.7.6.3p14: K&R-style un-prototyped declaration or
    // function definition having no parameters.
    // We return a type representing K&R-style declaration here.
    // If this is actually part of a declartion, the type will be fixed later.
    if (is_keyword(tok, stringify(")")))
        return make_func_type(rettype, zero);
    if (next_token(p, offset, sym(...)))
        error(p2, "at least one parameter is required before \"...\"");
    if (is_type(p, token(p, offset))) {
        boolean ellipsis;
        vector paramtypes = zero;
        read_declarator_params(p, offset, env, paramtypes);
        return make_func_type(rettype, paramtypes);
    }
    vector paramtypes = zero;

    // a param is an object with a type
    //  for (int i = 0; i < vector_length(paramvars); i++)
    // push(paramtypes, pget(env, sym(type), sym(int)));
    
    return make_func_type(rettype, paramtypes);
}

static result read_declarator_tail(parser p, index offset, scope env, Type basety);


static result read_declarator_array(parser p, index offset, scope env, Type basety) {
    int len;
    if (next_token(p, offset, stringify("]"))) {
        len = -1;
    } else {
        len = read_intexpr(p, offset);
        expect(p, offset, stringify("}"));
    }
    result t = read_declarator_tail(p, offset, env, basety);
    if (pget(t.v, sym(kind)) == sym(func))
        error(p, "array of functions");
    return make_array_type(t.v, len);
}

static result read_declarator_tail(parser p, index offset, scope env, Type basety) {
    if (next_token(p, offset, stringify("{")))
        return read_declarator_array(p, offset, env, basety);
    if (next_token(p, offset, stringify("(")))
        return read_func_param_list(p, offset, env, basety);
    return res(basety, offset);
}

vector read_decl_init(parser p, index offset, scope env, Type ty) {
    vector r = 0;
    if (is_keyword(token(p, offset), stringify("{")) || is_string(ty)) {
        read_initializer_list(p, offset, env, r, ty);
    } else {
        result init = conv(p, read_assignment_expr(p, offset, env));
        if (is_inttype(pget(init.v, sym(type))) && pget(init.v, sym(type), sym(kind)) != pget(ty, sym(kind)))
            init = ast_conv(ty, init.v);
        // push(r, ast_init(init, ty));
    }
    return r;
}

// C11 6.7.6: Declarators
result read_declarator(parser p, index offset, scope env, buffer *rname, Type basety) {
    if (next_token(p, offset, stringify("("))) {
        // "(" is either beginning of grouping parentheses or of a function parameter list.
        // If the next token is a type name, a parameter list must follow.
        if (is_type(p, token(p, offset)))
            return read_func_param_list(p, offset, env, basety);
        // stub is here to handle some issues with function declarations
        // and grouping
        Type stub = timm("kind", sym(stub)); // stub type
        Type t = read_declarator(p, offset, env, rname, stub, params);
        expect(p, offset, stringify(")"));
        stub = read_declarator_tail(p, offset, env, basety);
        return t;
    }
    if (next_token(p, offset, sym(*))) {
        // where is this being denoted?
        return read_declarator(p, offset, env, rname, make_ptr_type(basety), params);
    }
    tuple tok = token(p, offset);
    if (pget(tok, sym(kind)) == sym(identifier)) {
        *rname = pget(tok, sym(sval));
        return read_declarator_tail(p, offset, env, basety);
    }
    //    error(p, "identifier, ( or * are expected, but got", tok);
    return read_declarator_tail(p, offset, env, basety);
}

// this has to be jiggered to include function declarations - there was
// a scan-ahead-and-unget-tokens loop before
index read_decl(parser p, index offset, scope env, vector block) {
    printf("read decl\n");
    string sclass = 0;
    int isstatic = 0;
    Type basetype = read_decl_spec(p, offset, env, sclass);
    if (next_token(p, offset, stringify(";")))
        return offset;

    buffer name = zero;
    Type ty = read_declarator(p, offset, env, &name, basetype, zero);
    // why do we care ..storage scope
    //        if (sclass == sym(static)) {
    //            set(ty, sym(isstatic), sym(true));
    //        }
    
    // there was some special handling to assign a global for static locals
    
    if (sclass == sym(typedef)) {
        Node r = timm("kind", sym(typedef), "type", ty);
        // no - typedefs are scoped
        // set(p->global, name, r);
    } else {
        Node var = ast_var(env, ty, name);
        if (next_token(p, offset, sym(=))) {
            //push(block, ast_decl(var, read_decl_init(p, env, ty)));
        } else if (sclass != sym(extern) && pget(ty, sym(kind)) != sym(func)) {
            //push(block, ast_decl(var, zero));
        }
    }
    
    if (next_token(p, offset, stringify(";"))) return offset;
    
    if (!next_token(p, offset, stringify(":")))
        error(p, "';' or ',' are expected, but got %s", peek());
    return offset;
}

static Node read_func_body(parser p, index offset, scope env, Type functype, buffer fname, vector params) {
    // functype? what about params?
    scope s = allocate_scope(env,
                             "__func__", funcname,
                             "__FUNCTION__", funcname);  // collect them all!

    Node funcname = ast_string(p, env, fname);
    Node body = read_compound_stmt(p, offset, s);
    return timm("kind", sym(func),
                "type", functype,
                "name", fname,
                "parms", params,
                //                "localvars", localvars,
                "body", body);
}

static Node read_funcdef(parser p, index offset, scope env) {
    string sclass = 0;
    Type basetype = read_decl_spec(p, offset, env, sclass);
    //    push_scope(p);
    buffer name;
    vector params = 0;
    // probably dont need to pass params
    Type functype = read_declarator(p, offset, env, &name, basetype, params);
    // why do we care? make sure there is a file scope
    //  set(functype->isstatic = (sclass == sym(static));
    ast_var(env, functype, name);
    expect(p, offset, stringify("{"));
    Node r = read_func_body(p, offset, env, functype, name, params);
    return r;
}

