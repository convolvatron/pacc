#include <pacc.h>

#define iserror(__r) (!(__r).success)

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


typedef result(*reader) (parser, u64, scope);

// parameterize separator or terminal (a, b, c) vs {a; b; c;}?
result listof(parser p, u64 offset, scope env, 
              value separator, value terminus, reader each)
{
    nursery n = allocate_nursery(10);
    while (offset < nzv(p->tokens)) {
        // chain environments?
        result e = each(p, offset, env);
        if (iserror(e)) return e;
        offset = e.offset;
        push_mut(n, &e.v, bitsizeof(value));
        value tok = token(p, offset);
        if (!is_keyword(tok, separator)) {
            if (is_keyword(tok, terminus)) {
                return res(vector_from_nursery(n), offset);
            }
            return error("expected separator or terminus got tok", separator, terminus, tok);
        }
    }
    return error("unterminated grouping starting", offset);
}



int read_intexpr(parser p, index offset) {
    // xxx - we were doing static evaluation here...pass through
    // take that back, this gets used for array bounds and such
    return 0;
}

result struct_field(parser p, index offset, scope env)
{
    // ? right?
    return read_declaration(p, offset, env);
    //      if (next_token(p, stringify(":")))
    // just change to readintexpr - there were some checks
    //        set(fieldtype, sym(bitsize), read_bitsize(p, name, fieldtype));
}

result read_decl_spec(parser p, index offset, scope env)
{
    value tok = token(p, offset);
    value k = pget(tok, sym(kind));
    value id = pget(tok, sym(value));    
    
    if (k == sym(identifier)){
        value def = pget(env, sym(types), pget(tok, sym(value)));
        if (def) return res(def, offset+1);
    }
    
    if ((id == sym(struct)) || (id == sym(union))) {
        value tag = zero;
        tuple tok = token(p, offset);
        value r;
        
        if (get(tok, sym(kind)) == sym(identifier)) {
            tag = pget(env, sym(structs), tag);
        } 
        
        result basetype = read_decl_spec(p, offset, env);
        // seems odd
        //        if (pget(basetype, sym(kind)) == sym(struct) && next_token(p, sym(;)))
        // push(r, timm(type, basetype));
        //            continue;
        //        }
        // clearly we deconstructed something a little too far here
        //    if (next_token(p, offset, stringify(","))) continue;
        
        
        // seems like expect could be broadened. also float the semis up
        if (is_keyword(token(p, offset), stringify("}"))) {
            error("missing ';' at the end of field list", v);
        } else expect(p, offset, stringify(";"));
        
        //        timm("name", name, "type", fieldtype);
        
        expect(p, offset, stringify("}"));
        // umm..       (!next_token(p, offset, stringify("{")))?zero:read_rectype_fields_sub(p, offset, env));    
        return res(timm("fields", zero), offset);
    }
    
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
    
    error("type mismatch", tok);
    return res(zero, 0);
}


static result read_func_param_list(parser p, index offset, scope env, Type rettype) {
    // C11 6.7.6.3p10: A parameter list with just "void" specifies that
    // the function has no parameters.
    tuple tok = token(p, offset);
    if (is_keyword(tok, sym(void)) && next_token(p, offset, stringify(")")))
        return res(make_func_type(rettype, zero), offset);
    
    // C11 6.7.6.3p14: K&R-style un-prototyped declaration or
    // function definition having no parameters.
    // We return a type representing K&R-style declaration here.
    // If this is actually part of a declartion, the type will be fixed later.
    if (is_keyword(tok, stringify(")")))
        return res(make_func_type(rettype, zero), offset);
    if (next_token(p, offset, sym(...)))
        error("at least one parameter is required before \"...\"");
    if (is_type(p, token(p, offset))) {
        boolean ellipsis;
        vector paramtypes = zero;
        int length;
        
        for (;;) {
            tuple tok = token(p, offset);
            if (next_token(p, offset, sym(...))) {
                // if (!types) error("at least one parameter is required before \"...\"");
                expect(p, offset, stringify(")"));
                goto construct;
            }
            result basety = read_decl_spec(p, offset, env);
            
            //    if (!basety.v) {
            //        // defer
            //        if (!optional) {
            //            error(p, "type expected, but got", peek());
            //        }
            //    }
            
            result ty = read_declarator(p, basety.offset, env, basety.v);
            
            // C11 6.7.6.3p7: Array of T is adjusted to pointer to T
            // in a function parameter list.
            //            if (pget(ty.v, sym(kind)) == sym(array))
            //                return res(make_ptr_type(pget(ty.v, sym(ptr))), offset);
            // C11 6.7.6.3p8: Function is adjusted to pointer to function
            // in a function parameter list.
            //            if (pget(ty.v, sym(kind)) == sym(func))
            //                return res(make_ptr_type(ty.v), offset);
            
            tok = token(p, offset);
            if (is_keyword(tok, stringify(")"))) {
                offset++;
                goto construct;
            }
            
            if (!is_keyword(tok, stringify(",")))
                error("stringify(",") expected, but got %s", tok);
        }
    construct:
        return res(zero, offset); // build vector        
        
        return res(make_func_type(rettype, paramtypes), 0);
    }
    vector paramtypes = zero;

    // a param is an object with a type
    //  for (int i = 0; i < vector_length(paramvars); i++)
    // push(paramtypes, pget(env, sym(type), sym(int)));
    
    return res(make_func_type(rettype, paramtypes), offset);
}

static result read_declarator_tail(parser p, index offset, scope env, Type basety) {
    if (next_token(p, offset, stringify("{"))) {
        int len;
        if (next_token(p, offset, stringify("]"))) {
            len = -1;
        } else {
            len = read_intexpr(p, offset);
            expect(p, offset, stringify("}"));
        }
        result t = read_declarator_tail(p, offset, env, basety);
        if (pget(t.v, sym(kind)) == sym(func))
            error("array of functions");
        
        return res(make_array_type(t.v, len), offset);
    }
    //    if (next_token(p, offset, stringify("(")))
    return res(basety, offset);
}

result funcparam(parser p, index offset, scope env)
{
    return res(zero, offset);
}

    
// C11 6.7.6: Declarators
result read_declarator(parser p, index offset, scope env, Type basety) {
    if (next_token(p, offset, stringify("("))) {
        // "(" is either beginning of grouping parentheses or of a function parameter list.
        // If the next token is a type name, a parameter list must follow.
        if (is_type(p, token(p, offset)))
            return listof(p, offset, env, (value)',', (value)')', funcparam);
        // stub is here to handle some issues with function declarations
        // and grouping
        Type stub = timm("kind", sym(stub)); // stub type
        result t = read_declarator(p, offset, env, stub);
        expect(p, offset, stringify(")"));
        read_declarator_tail(p, offset, env, basety);
        return t;
    }
    if (next_token(p, offset, sym(*))) {
        // where is this being denoted?
        return read_declarator(p, offset, env, make_ptr_type(basety));
    }
    tuple tok = token(p, offset);
    if (pget(tok, sym(kind)) == sym(identifier)) {
        //        *rname = pget(tok, sym(value));
        return read_declarator_tail(p, offset+1, env, basety);
    }
    //    error(p, "identifier, ( or * are expected, but got", tok);
    return read_declarator_tail(p, offset, env, basety);
}


// this has to be jiggered to include function declarations - there was
// a scan-ahead-and-unget-tokens loop before
result read_declaration(parser p, index offset, scope env)
{
    result basetype = read_decl_spec(p, offset, env);
    
    result ty = read_declarator(p, basetype.offset, env, basetype.v);
    outputline(stringify("read declaration declarator"), print(ty.v));
    if (iserror(ty)) return ty;
    
    // there was some special handling to assign a global name for static locals
    string sclass = zero;
    if (sclass == sym(typedef)) {
        Node r = timm("kind", sym(typedef), "type", ty);
        // no - typedefs are scoped
        // set(p->global, name, r);
        return error("no typedef yet");
    }
    
    // initializer
    if (next_token(p, offset, sym(=))) {
        vector r = 0;
        if (is_keyword(token(p, offset), stringify("{")) || is_string(ty.v)) {
            tuple tok = token(p, offset);
            value k = pget(tok, sym(kind));
            value v = pget(tok, sym(value));
            
            if (is_string(ty.v)) {
                if (pget(tok, sym(kind)) == sym(string)) {
                    return res(zero, offset);
                }
                if (is_keyword(tok, stringify("{")) && (pget(env, sym(kind)) == sym(string))) {
                    tok = token(p, offset);
                    expect(p, offset + 1, stringify("}"));
                    return res(v, offset + 1);
                }
            }
            offset--;
            string tk = pget(ty.v, sym(kind));
            if (tk == sym(array)) {
                boolean has_brace = toboolean(next_token(p, offset, stringify("{")));
                int i;
                vector v = 0;
                
                // we were counting to match the input type
                for (i = 0; ; i++) {
                    tuple tok = token(p, offset);
                    // wrapper functions?
                    if (is_keyword(tok, stringify("}"))) {
                        return res(tok, offset);
                    }
                    
                    if ((is_keyword(tok, sym(.)) || is_keyword(tok, stringify("{")))){
                        offset--;
                        return res(tok, offset);
                    }
                    
                    if (is_keyword(tok, stringify("{"))) {
                        int idx = read_intexpr(p, offset);
                        //      if (idx < 0 || (!flexible && ty->len <= idx))
                        //        error(p, "array designator exceeds array bounds: %d", idx);
                        i = idx;
                        expect(p, offset, stringify("}"));
                    }
                    //        read_initializer_elem(p, env, offset);
                    next_token(p, offset, stringify(","));
                    //    if (has_brace) expect(p, offset, stringify("}"));
                    // finish:
                    // is this is a default int thing?
                    //    if (!pget(ty, sym(len))) {
                    //        set(ty, sym(len), value_from_u64(i));
                    //        set(ty, sym(size), value_from_u64(u64_from_value(elemsize) * i));
                    //    }
                }
                if (tk == sym(struct)) {
                    boolean has_brace = toboolean(next_token(p, offset, stringify("{")));
                    int i = 0;
                    
                    tuple tok = token(p, offset);
                    if (is_keyword(tok, stringify("}"))) return res(zero, offset + 1);
                    
                    // there was something about nested bracket initializers
                    if (is_keyword(tok, sym(.))) {
                        tok = token(p, ++offset);
                        if (!tok || pget(tok, sym(kind)) != sym(identifier))
                            return error("malformed desginated initializer: %s", tok);
                    }
                    //read_initializer_elem(p, env, offset);
                    next_token(p,  ++offset, stringify(","));
                    return res(0, offset);
                } else {
                    result init = read_assignment_expr(p, offset, env);
                    // later 
                    //   if (is_inttype(pget(init.v, sym(type))) && pget(init.v, sym(type), sym(kind)) != pget(ty.v, sym(kind)))
                    //    init.v = ast_conv(ty.v, init.v);
                    // push(r, ast_init(init, ty));
                }
                return res(0, offset);
            } else if (sclass != sym(extern) && pget(ty.v, sym(kind)) != sym(func)) {
                //push(block, ast_decl(var, zero));
            }
            Node var = ast_var(env, ty.v, zero);
        }
    }    
    if (next_token(p, ty.offset, stringify(";"))) return res(zero,ty.offset+1);
    
    if (!next_token(p, offset, stringify(":")))
        return error("';' or ',' are expected, but got %s", peek());
    
    return res(zero, ty.offset);
}


