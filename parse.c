// Copyright 2012 Rui Ueyama. Released under the MIT license.
#include "pacc.h"
#include <stdlib.h>

void errorf(void *x, char *fmt, ...);


#define allocate_scope(...) true   

boolean is_keyword(tuple tok, string x)
{
    return toboolean((get(tok, sym(kind)) == sym(keyword)) &&
                     (get(tok, "value") == x));
}


void errorf(void *x, char *format, ...)
{
    halt("parse error");
}

tuple token(parser p)
{
    value v;
    if (p->readahead) {
        v = p->readahead;
        p->readahead = 0;
    } else {
        v = get_token(p->lex);
        // this is supposed to be a catchall, but wondering if some non-deterministic
        // check might stumble on this
        if (pget(v, sym(kind)) == sym(eof)) 
            error(p, "premature end of input");        
    }
    return v;
}

// we care about the ordering, so a map and a vector..
Type lookup_field(Type t, value s)
{
    return 0;
}

Type lookup_index(Type t, int x)
{
    return 0;
}

static Node ast_var(scope s, Type ty, string name) {
    return timm("kind", sym(variable), "type", ty, "name", name);
}


#define get table_get

typedef value vector;
void allocate_vector();

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

static Type make_ptr_type(Type ty) {
    return timm("kind", sym(ptr), "ptr", ty);
}

static Type make_func_type(Type rettype, vector paramtypes, boolean has_varargs) {
    return timm("kind", sym(func),
                "rettype", rettype,
                "params", paramtypes,
                "hasva", has_varargs);
}


static boolean is_string(Type ty) {
    return toboolean((pget(ty, sym(kind)) == sym(array)) &&
                     (pget(ty, sym(ptr), sym(kind)) == sym(char)));
}



void assign_string(parser p, vector inits, Type ty, buffer s) {
}
#if 0
if (ty->len == -1)
    ty->len = ty->size = buffer_length(s);
int i = 0;
for (; i < ty->len && *p; i++)
    push(inits, ast_init(ast_int_listeral(p, p->type_char, *p++), p->type_char, off + i));
for (; i < ty->len; i++)
    push(inits, ast_init(ast_int_literal(p, p->type_char, 0), p->type_char, off + i));
}
#endif

static void read_initializer_elem(parser p, scope env, vector inits, Type ty, boolean designated);

void read_struct_initializer(parser p, scope env, vector inits, Type ty, boolean designated) {
    boolean has_brace = toboolean(next_token(p, stringify("{")));
    int i = 0;

    for (;;) {
        tuple tok = token(p);
        if (is_keyword(tok, stringify("}"))) {
            //      if (!has_brace)
            //        unget_token(tok);
            return;
        }

        Type fieldtype;
        if ((is_keyword(tok, sym(.)) || is_keyword(tok, stringify("{"))) && !has_brace && !designated) {
            //      unget_token(tok);
            return;
        }

        if (is_keyword(tok, sym(.))) {
            tok = token(p);
            if (!tok || pget(tok, sym(kind)) != sym(identifier))
                error(p, "malformed desginated initializer: %s", tok);
            fieldtype = lookup_field(ty, pget(tok, sym(value)));
            if (!fieldtype)
                error(p, "field does not exist: %s", tok);
            designated = true;
        } else {
            fieldtype = lookup_index(ty, i++);
        }
        // off? really?
        read_initializer_elem(p, env, inits, fieldtype, designated);
        next_token(p, stringify(","));
        designated = false;
        if (pget(ty, sym(kind)) != sym(struct))
            break;
    }
    if (has_brace) expect(p, stringify("}"));
}



static int read_intexpr(parser p) {
    // xxx - we were doing static evaluation here...pass through
    // take that back, this gets used for array bounds and such
    return 0;
}

static void read_array_initializer(parser p, scope env, vector inits, Type ty, boolean *designated) {
    boolean has_brace = toboolean(next_token(p, stringify("{")));
    boolean flexible = toboolean(pget(ty, sym(len)) <= 0);
    value elemsize = pget(ty, sym(ptr), sym(size));
    int i;
    
    for (i = 0; flexible || i < u64_from_value(pget(ty, sym(len))); i++) {
        tuple tok = token(p);
        // wrapper functions?
        if (is_keyword(tok, stringify("}"))) {
            if (!has_brace) unget(p, tok);
            return;
        }
        
        if ((is_keyword(tok, sym(.)) || is_keyword(tok, stringify("{"))) && !has_brace && !designated) {
            unget(p, tok);
            return;
        }
        
        if (is_keyword(tok, stringify("{"))) {
            int idx = read_intexpr(p);
            //      if (idx < 0 || (!flexible && ty->len <= idx))
            //        error(p, "array designator exceeds array bounds: %d", idx);
            i = idx;
            expect(p, stringify("}"));
            *designated = true;
        }
        read_initializer_elem(p, env, inits, pget(ty, sym(ptr)), designated);
        next_token(p, stringify(","));
        *designated = false;
    }
    if (has_brace) expect(p, stringify("}"));
    // finish:
    // is this is a default int thing?
    //    if (!pget(ty, sym(len))) {
    //        set(ty, sym(len), value_from_u64(i));
    //        set(ty, sym(size), value_from_u64(u64_from_value(elemsize) * i));
    //    }
}

// fork out struct and array?
static void read_initializer_list(parser p,
                                  scope env,
                                  vector inits,
                                  Type ty,
                                  boolean designated) {
    tuple tok = token(p);
    value k = pget(tok, sym(kind));
    value v = pget(tok, sym(value));

    if (is_string(ty)) {
        if (pget(tok, sym(kind)) == sym(string)) {
            assign_string(p, inits, ty, v);
            return;
        }
        if (is_keyword(tok, stringify("{")) && (pget(p->global, sym(kind)) == sym(string))) {
            tok = token(p);
            assign_string(p, inits, ty, v);
            expect(p, stringify("}"));
            return;
        }
    }
    unget(p, tok);
    string tk = pget(ty, sym(kind));
    if (tk == sym(array)) {
        read_array_initializer(p, env, inits, ty, designated);
    } else if (tk == sym(struct)) {
        read_struct_initializer(p, env, inits, ty, designated);
    } else {
        Type arraytype = make_array_type(ty, 1);
        read_array_initializer(p, env, inits, arraytype, designated);
    }
}

static vector read_decl_init(parser p, scope env, Type ty) {
    vector r = 0;
    if (is_keyword(token(p), stringify("{")) || is_string(ty)) {
        read_initializer_list(p, env, r, ty, false);
    } else {
        Node init = conv(p, read_assignment_expr(p, env));
        if (is_inttype(pget(init, sym(type))) && pget(init, sym(type), sym(kind)) != pget(ty, sym(kind)))
            init = ast_conv(ty, init);
        // push(r, ast_init(init, ty));
    }
    return r;
}

static Node read_compound_literal(parser p, scope env, Type ty) {
    buffer name = make_label();
    vector init = read_decl_init(p, env, ty);
    Node r = ast_var(env, ty, name);
    // set(r, sym(init), init);
    return r;
}


boolean next_token(parser p, string kind) {
    tuple tok = token(p);
    if (is_keyword(tok, kind)){
        return true;
    }
    unget(p, tok);
    return false;
}

static boolean same_arith_type(Type t, Type u) {
    return toboolean(get(t, sym(kind)) == get(u, sym(kind)) && get(t, sym(usig)) == get(u, sym(usig)));
}

#if 0
// xxx little state machines - use the set trick
"u", "u32",
"ul", "u64",
"ull", "u64",
"l", "s64", // original source has u64
"ll", "s64"
"llu", "u64"

i guess no lul !
#endif
    


static buffer read_rectype_tag(parser p) {
    tuple tok = token(p);
    if (get(tok, sym(kind)) == sym(identifier)) {
        return get(tok, sym(value));
    }
    return zero;
}


Node read_assignment_expr(parser p, scope env);

#if 0
static value read_alignas(parser p) {
    // C11 6.7.5. Valid form of _Alignof is either _Alignas(type-name) or
    // _Alignas(constant-expression).
    expect(p, stringify("("));
    value r = is_type(p, token(p))
        ? pget(read_cast_type(p, env), sym(align))
        : read_intexpr(p);
    expect(p, stringify(")"));
    return r;
}


// and .. some of that other crap
static Type storage_class(parser p)
{
    if ((id != sym(const)) &&
        (id != sym(volatile)) &&
        (id != sym(inline)) &&
        (id != sym(noreturn))) {
        return type;
    }
    // construct metadata
            if (id == sym(typedef)) {
                if (sclass) goto err;
                sclass = sym(typedef);
            }
            if (id == sym(extern)) {
                if (sclass) goto err;
                sclass = sym(extern);
            }
            if (id == sym(static)){
                if (sclass) goto err;
                sclass = sym(static);
            }
            if (id == sym(auto)) {
                if (sclass) goto err;
                sclass = sym(auto);
            }
            if (id == sym(register)) {
                if (sclass) goto err; sclass = sym(register);
            }    
}
#endif


static Type read_rectype_def(parser p, scope env, string kind);

Type read_decl_spec(parser p, scope env, string *rsclass) {
    value tok = token(p);
    value k = pget(tok, sym(kind));
    
    if (k == sym(identifier)){
        Type def = get_typedef(p->global, pget(tok, sym(value)));
        if (def) return def;
    }
    
    value id = pget(token, sym(id));
    
    if ((id == sym(struct)) || (id == sym(union)))
        return read_rectype_def(p, env, id); 

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
    return 0;
}

static vector read_rectype_fields_sub(parser p, scope env) {
    tuple r = 0;
    for (;;) {
        if (!is_type(p, token(p))) break; //??

        Type basetype = read_decl_spec(p, env, zero);
        if (pget(basetype, sym(kind)) == sym(struct) && next_token(p, sym(;))) {
            // push(r, timm(type, basetype));
            continue;
        }
        for (;;) {
            buffer name = zero;
            // DECL_PARAM_TYPEONLY);
            Type fieldtype = read_declarator(p, env, &name, basetype, zero);
            fieldtype = allocate_scope(fieldtype);
            
            //      if (next_token(p, stringify(":")))
            //        set(fieldtype, sym(bitsize), read_bitsize(p, name, fieldtype));
            
            if (next_token(p, stringify(","))) continue;

            // seems like expect could be broadened
            value v;
            if (is_keyword(v = token(p), stringify("}"))) {
                error(p, "missing ';' at the end of field list", v);
            } else
                expect(p, stringify(";"));

            timm("name", name, "type", fieldtype);
        }
    }
    expect(p, stringify("}"));
    return r; // vector of fields
}

static Type read_rectype_def(parser p, scope env, string kind) {
    buffer tag = read_rectype_tag(p);
    Type r;
    if (tag) {
        r = pget(p->global, sym(tags), tag);
        if (r && (pget(r, sym(kind)) == sym(enum) || pget(r, sym(kind)) != kind))
            error(p, "declarations of %s does not match", tag);
        if (!r) {
            r = timm("kind", kind,
                     "tags", r); // ?
        }
    } else {
        r = timm("kind", kind);
    }
    return timm("fields",
                (!next_token(p, stringify("{")))?zero:read_rectype_fields_sub(p, env));
}


static value read_bitsize(parser p, buffer name, Type ty) {
    if (!is_inttype(ty))
        error(p, "non-integer type cannot be a bitfield: %s", ty2s(ty));

    int r = read_intexpr(p);
    int maxsize = u64_from_value(pget(ty, value_from_u64(sym(kind) == sym(boolean) ? 1 :
                                                         u64_from_value(pget(ty, sym(size))) * 8)));
    if (r < 0 || maxsize < r)
        error(p, "invalid bitfield size for %s: %d", ty2s(ty), r);
    if (r == 0 && name != zero)
        error(p, "zero-width bitfield needs to be unnamed: %s", name);
    return value_from_u64(r);
}

Node read_unary_expr(parser p, scope env);

#define vector_length(_x) 1

static Node read_assignment_expr(parser p, scope env);

static vector read_func_args(parser p, scope env, vector params) {
    vector args = 0; // finalize...we can have a larva, write or read, lets not today
    int i = 0;
    for (;;) {
        if (next_token(p, stringify(")"))) break;
        
        Node arg = conv(p, read_assignment_expr(p, env));
        Type ty = pget(arg, sym(type));
        Type paramtype;
        
        // why dont we just unify this later?
        if (i < vector_length(params)) {
            paramtype = pget(params, i++);
        } else {
            // default types?
            paramtype =
                is_inttype(ty) ? pget(p->global, sym(types), sym(int)) :
                pget(arg, sym(type));
        }
        
        // ensure_assignable(paramtype, ty);
        if (pget(paramtype, sym(kind)) != pget(arg, sym(type), sym(kind)))
            arg = ast_conv(paramtype, arg);

        //args = push(args, arg);

        tuple tok = token(p);
        if (is_keyword(tok, stringify(")"))) break;
        if (!is_keyword(tok, stringify(",")))
            error(p, "unexpected token: '%s'", tok);
    }
    return args;
}

static Node read_funcall(parser p, scope env, Node fp) {
    if (pget(fp, sym(kind)) == sym(addr) && pget(fp, sym(operand), sym(kind)) == sym(funcdesg)) {
        Node desg = pget(fp, sym(operand));
        vector args = read_func_args(p, env, pget(desg, sym(type), sym(parameters)));
        //        ast_funcall(Type ftype, buffer fname, vector args)
        return timm("kind", sym(funcall),
                    "type", pget(desg, sym(type), sym(rettype)), // rettype
                    "name", pget(desg, sym(name)),
                    "args", args,
                    "ftype", pget(desg, sym(type))); // we need this why?
    }
    vector args = read_func_args(p, env, pget(fp, sym(type), sym(ptr), sym(parameters)));
    // this is not a separate thing    
    //    ast_funcptr_call(fp, args);
    return timm("kind", sym(funcptr_call), "type",
                pget(fp, sym(type), sym(ptr), sym(rettype)),
                "fptr", fp, 
                "args", args);
}

static string get_compound_assign_op(tuple tok) {
    if (pget(tok, sym(kind)) != sym(keyword))
        return 0;
    return(pget(tok, sym(id)));
}

static void read_initializer_elem(parser p, scope env, vector inits, Type ty, boolean designated) {
    next_token(p, sym(=));
    if (pget(ty, sym(kind)) == sym(array) || pget(ty, sym(kind)) == sym(struct)) {
        read_initializer_list(p, env, inits, ty, designated);
    } else if (next_token(p, stringify("{"))) {
        read_initializer_elem(p, env, inits, ty, true);
        expect(p, stringify("}"));
    } else {
        Node expr = conv(p, read_assignment_expr(p, env));
        // ensure_assignable(ty, pget(expr, sym(type)));
        // push(inits, ast_init(expr, ty));
    }
}


static Type read_func_param(parser p, scope env, buffer *name, boolean optional) {
    string sclass = 0;
    Type basety = pget(p->global, sym(type), sym(int));
    
    if (is_type(p, token(p))) {
        basety = read_decl_spec(p, env, &sclass);
    } else if (optional) {
        error(p, "type expected, but got", peek());
    }

    // , optional ? DECL_PARAM_TYPEONLY : DECL_PARAM);
    Type ty = read_declarator(p, env, name, basety, zero);

    // C11 6.7.6.3p7: Array of T is adjusted to pointer to T
    // in a function parameter list.
    if (pget(ty, sym(kind)) == sym(array))
        return make_ptr_type(pget(ty, sym(ptr)));
    // C11 6.7.6.3p8: Function is adjusted to pointer to function
    // in a function parameter list.
    if (pget(ty, sym(kind)) == sym(func))
        return make_ptr_type(ty);
    return ty;
}

// Reads an ANSI-style prototyped function parameter list.
void read_declarator_params(parser p, scope env, vector types, vector vars, boolean *ellipsis) {
    boolean typeonly = toboolean(!vars);
    *ellipsis = false;
    for (;;) {
        tuple tok = token(p);
        if (next_token(p, sym(...))) {
            if (!types) error(p, "at least one parameter is required before \"...\"");
            expect(p, stringify(")"));
            *ellipsis = true;
            return;
        }
        buffer name;
        Type ty = read_func_param(p, env, &name, typeonly);
        // ensure_not_void(ty);
        // push(types, ty);
        //        if (!typeonly)
        //            push(vars, ast_var(env, ty, name));
        tok = token(p);
        if (is_keyword(tok, stringify(")")))
            return;
        if (!is_keyword(tok, stringify(",")))
            error(p, "stringify(",") expected, but got %s", tok);
    }
}

static Type read_func_param_list(parser p, scope env, vector paramvars, Type rettype) {
    // C11 6.7.6.3p10: A parameter list with just "void" specifies that
    // the function has no parameters.
    tuple tok = token(p);
    if (is_keyword(tok, sym(void)) && next_token(p, stringify(")")))
        return make_func_type(rettype, zero, false);

    // C11 6.7.6.3p14: K&R-style un-prototyped declaration or
    // function definition having no parameters.
    // We return a type representing K&R-style declaration here.
    // If this is actually part of a declartion, the type will be fixed later.
    if (is_keyword(tok, stringify(")")))
        return make_func_type(rettype, zero, true);
    //  unget_token(tok);

    if (next_token(p, sym(...)))
        error(p2, "at least one parameter is required before \"...\"");
    if (is_type(p, token(p))) {
        boolean ellipsis;
        vector paramtypes = zero;
        read_declarator_params(p, env, paramtypes, paramvars, &ellipsis);
        return make_func_type(rettype, paramtypes, ellipsis);
    }
    if (!paramvars)
        error(p, "invalid function definition");
    vector paramtypes = zero;

    // a param is an object with a type
    //  for (int i = 0; i < vector_length(paramvars); i++)
    // push(paramtypes, pget(p->global, sym(type), sym(int)));
    
    return make_func_type(rettype, paramtypes, false);
}

static Type read_declarator_tail(parser p, scope env, Type basety, vector params);


static Type read_declarator_array(parser p, scope env, Type basety) {
    int len;
    if (next_token(p, stringify("]"))) {
        len = -1;
    } else {
        len = read_intexpr(p);
        expect(p, stringify("}"));
    }
    Type t = read_declarator_tail(p, env, basety, zero);
    if (pget(t, sym(kind)) == sym(func))
        error(p, "array of functions");
    return make_array_type(t, len);
}

static Type read_declarator_func(parser p, scope env, Type basety, vector param) {
    return read_func_param_list(p, env, param, basety);
}


static Type read_declarator_tail(parser p, scope env, Type basety, vector params) {
    if (next_token(p, stringify("{")))
        return read_declarator_array(p, env, basety);
    if (next_token(p, stringify("(")))
        return read_declarator_func(p, env, basety, params);
    return basety;
}

#if 0
static void skip_type_qualifiers(parser p) {
    while (next_token(p, sym(const)) ||
           next_token(p, sym(volatile)) ||
           next_token(p, sym(RESTRICT)));
}
#endif

// C11 6.7.6: Declarators
Type read_declarator(parser p, scope env, buffer *rname, Type basety, vector params) {
    if (next_token(p, stringify("("))) {
        // "(" is either beginning of grouping parentheses or of a function parameter list.
        // If the next token is a type name, a parameter list must follow.
        if (is_type(p, token(p)))
            return read_declarator_func(p, env, basety, params);
        
        // If not, it's grouping. In that case we have to read from outside.
        // For example, consider int (*)(), which is "pointer to function returning int".
        // We have only read "int" so far. We don't want to pass "int" to
        // a recursive call, or otherwise we would get "pointer to int".
        // Here, we pass a dummy object to get "pointer to <something>" first,
        // continue reading to get "function returning int", and then combine them.
        Type stub = timm("kind", sym(stub)); // stub type
        Type t = read_declarator(p, env, rname, stub, params);
        expect(p, stringify(")"));
        stub = read_declarator_tail(p, env, basety, params);
        return t;
    }
    if (next_token(p, sym(*))) {
        // where is this being denoted?
        return read_declarator(p, env, rname, make_ptr_type(basety), params);
    }
#if 0    
    tuple tok = token(p);
    if (pget(tok, sym(kind)) == sym(identifier)) {
        if (ctx == DECL_CAST)
            error(p, "identifier is not expected, but got", tok);
        *rname = pget(tok, sym(sval));
        return read_declarator_tail(p, env, basety, params);
    }
    if (ctx == DECL_BODY || ctx == DECL_PARAM)
        error(p, "identifier, ( or * are expected, but got", tok);
#endif    
    //  unget_token(tok);
    return read_declarator_tail(p, env, basety, params);
}

// this has to be jiggered to include function declarations - there was
// a scan-ahead-and-unget-tokens loop before
void read_decl(parser p, scope env, vector block) {
    string sclass = 0;
    int isstatic = 0, isglobal = 0;
    Type basetype = read_decl_spec(p, env, &sclass);
    if (next_token(p, stringify(";")))
        return;
    for (;;) {
        buffer name = zero;
        // , DECL_BODY); 
        Type ty = read_declarator(p, env, &name, basetype, zero);
        // why do we care ..storage scope
        //        if (sclass == sym(static)) {
        //            set(ty, sym(isstatic), sym(true));
        //        }

        // there waws some special handling to assign a global for static locals
        
        // xxx - are all typedefs always global?
        
        if (sclass == sym(typedef)) {
            Node r = timm("kind", sym(typedef), "type", ty);
            // set(p->global, name, r);
        } else {
            Node var = ast_var((isglobal ? p->global : env), ty, name);
            if (next_token(p, sym(=))) {
                //push(block, ast_decl(var, read_decl_init(p, env, ty)));
            } else if (sclass != sym(extern) && pget(ty, sym(kind)) != sym(func)) {
                //push(block, ast_decl(var, zero));
            }
        }
        if (next_token(p, stringify(";"))) return;
        
        if (!next_token(p, stringify(":")))
            error(p, "';' or ',' are expected, but got %s", peek());
    }
}

static Node read_func_body(parser p, scope env, Type functype, buffer fname, vector params) {
    // functype? what about params?
    scope s = allocate_scope(env,
                             "__func__", funcname,
                             "__FUNCTION__", funcname);  // collect them all!

    Node funcname = ast_string(p, env, fname);
    Node body = read_compound_stmt(p, s);
    return timm("kind", sym(func),
                "type", functype,
                "name", fname,
                "parms", params,
                //                "localvars", localvars,
                "body", body);
}

static Node read_funcdef(parser p, scope env) {
    string sclass = 0;
    Type basetype = read_decl_spec(p, env, &sclass);
    //    push_scope(p);
    buffer name;
    vector params = 0;
    // , DECL_BODY);
    Type functype = read_declarator(p, env, &name, basetype, params);
    // why do we care? make sure there is a file scope
    //  set(functype->isstatic = (sclass == sym(static));
    ast_var(p->global, functype, name);
    expect(p, stringify("{"));
    Node r = read_func_body(p, env, functype, name, params);
    //  backfill_labels(p);
    return r;
}


vector read_toplevels(parser p, scope env) {
    vector top = 0;
    value v;
    // dont use token here, since we want to assume that eof anywhere else is an error
    while ((v = p->readahead) || ((v = get_token(p->lex)) && (pget(v, sym(kind))) != sym(eof))) {
        unget(p, v);
        read_decl(p, env, top);
    }
    return 0;
}

static void define_builtin(parser p, buffer name, Type rettype, vector paramtypes) {
    ast_var(p->global, make_func_type(rettype, paramtypes, false), name);
}

struct numeric {value name; int length; boolean has_sign;};

#define allocate_vector(...) false

#define slen(__x) (sizeof(__x)/sizeof(*__x))

// just buffer -> graph please
value parse(buffer b)
{
    parser p = malloc(sizeof(struct parser)); // xxx stdlib
    p->lex = create_lex(b);
    Type vt = timm("kind", sym(void));
    // set(pget(p->global, sym(types)), sym(void), vt);
    Type v = make_ptr_type(vt);
            
    struct numeric numtypes[] =  {{sym(boolean), 1, false},
                                  {sym(char), 8, true},
                                  {sym(short), 16, true},
                                  {sym(int), 32, true},
                                  {sym(signed), 32, true},
                                  {sym(unsigned), 32, false},                                  
                                  {sym(long), 64, true},
                                  {sym(llong), 128, true},
                                  {sym(uchar), 8, false},
                                  {sym(ushort), 16, false},
                                  {sym(uint), 32, false},
                                  {sym(ulong), 64, false},
                                  {sym(ullong), 128, false}};

    value types = allocate_table(slen(numtypes));
    // typedef
    for (int i = 0; i < slen(numtypes) ; i++) {
        table_insert(types, numtypes[i].name,
                     timm(sym(length), numtypes[i].length,
                          sym(signed), numtypes[i].has_sign));
    }

    p->global = timm(sym(types), types);
    value voidptr;

    define_builtin(p, sym(__builtin_return_address), v, voidptr);
    define_builtin(p, sym(__builtin_reg_class),
                   pget(p->global, sym(types), sym(int)),
                   voidptr);
    // parameter list
    define_builtin(p, sym(__builtin_va_arg), vt, allocate_vector(voidptr, voidptr));
    define_builtin(p, sym(__builtin_va_start), vt, allocate_vector(voidptr));
    read_toplevels(p, p->global);
    return 0;
}
