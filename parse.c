// Copyright 2012 Rui Ueyama. Released under the MIT license.
#include "pacc.h"
#include <stdlib.h>

// cache stringyify

#define read_until(__lex, __offset, __match, __tok )  \
    for (;tok = token(__lex, offset); is_keyword(tok, stringify("__match")); (*offset)++)

void errorf(void *x, char *fmt, ...);


boolean is_keyword(tuple tok, string x)
{
    printf("isk %d %p\n", (get(tok, sym(kind)) == sym(keyword)),
           (get(tok, sym(value))));
    return toboolean((get(tok, sym(kind)) == sym(keyword)) &&
                     (get(tok, sym("value")) == x));
}


void errorf(void *x, char *format, ...)
{
    halt("parse error");
}

Type lookup_index(Type t, int x)
{
    return 0;
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

static void read_initializer_elem(parser p, scope env, vector inits, Type ty, boolean designated, index offset);

// we can make a groupie - read_util (parser, reader)
index read_struct_initializer(parser p,
                              index offset,
                              scope env,
                              vector inits,
                              Type ty,
                              boolean designated) {
    boolean has_brace = toboolean(next_token(p, offset, stringify("{")));
    int i = 0;

    for (;;) {
        tuple tok = token(p, offset);
        if (is_keyword(tok, stringify("}"))) return offset + 1;

        Type fieldtype;
        if ((is_keyword(tok, sym(.)) || is_keyword(tok, stringify("{"))) && !has_brace && !designated) {
            return offset;
        }

        if (is_keyword(tok, sym(.))) {
            tok = token(p, ++offset);
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
        read_initializer_elem(p, env, inits, fieldtype, designated, offset);
        next_token(p,  ++offset, stringify(","));
        designated = false;
        if (pget(ty, sym(kind)) != sym(struct))
            break;
    }
    if (has_brace) expect(p, offset++,stringify("}"));
    return offset;
}



int read_intexpr(parser p, index offset) {
    // xxx - we were doing static evaluation here...pass through
    // take that back, this gets used for array bounds and such
    return 0;
}

static index read_array_initializer(parser p,
                                    scope env,
                                    vector inits,
                                    Type ty,
                                    boolean *designated,
                                    index offset) {
    
    boolean has_brace = toboolean(next_token(p, offset, stringify("{")));
    boolean flexible = toboolean(pget(ty, sym(len)) <= 0);
    value elemsize = pget(ty, sym(ptr), sym(size));
    int i;
    
    for (i = 0; flexible || i < u64_from_value(pget(ty, sym(len))); i++) {
        tuple tok = token(p, offset);
        // wrapper functions?
        if (is_keyword(tok, stringify("}"))) {
            if (!has_brace) offset--;
            return offset;
        }
        
        if ((is_keyword(tok, sym(.)) || is_keyword(tok, stringify("{"))) && !has_brace && !designated) {
            offset--;
            return offset;
        }
        
        if (is_keyword(tok, stringify("{"))) {
            int idx = read_intexpr(p, offset);
            //      if (idx < 0 || (!flexible && ty->len <= idx))
            //        error(p, "array designator exceeds array bounds: %d", idx);
            i = idx;
            expect(p, offset, stringify("}"));
            *designated = true;
        }
        read_initializer_elem(p, env, inits, pget(ty, sym(ptr)), designated, offset);
        next_token(p, offset, stringify(","));
        *designated = false;
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

// fork out struct and array?
static index read_initializer_list(parser p,
                           scope env,
                           vector inits,
                           Type ty,
                           boolean designated,
                           index offset) {
    tuple tok = token(p, offset);
    value k = pget(tok, sym(kind));
    value v = pget(tok, sym(value));

    if (is_string(ty)) {
        if (pget(tok, sym(kind)) == sym(string)) {
            assign_string(p, inits, ty, v);
            return offset;
        }
        if (is_keyword(tok, stringify("{")) && (pget(p->global, sym(kind)) == sym(string))) {
            tok = token(p, offset);
            assign_string(p, inits, ty, v);
            expect(p, offset + 1, stringify("}"));
            return offset + 1;
        }
    }
    offset--;
    string tk = pget(ty, sym(kind));
    if (tk == sym(array)) {
        read_array_initializer(p, env, inits, ty, designated, offset);
    } else if (tk == sym(struct)) {
        read_struct_initializer(p, offset, env, inits, ty, designated);
    } else {
        Type arraytype = make_array_type(ty, 1);
        read_array_initializer(p, env, inits, arraytype, designated, offset);
    }
    return offset;
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
    


static buffer read_rectype_tag(parser p, index offset) {
    tuple tok = token(p, offset);
    if (get(tok, sym(kind)) == sym(identifier)) {
        return get(tok, sym(value));
    }
    return zero;
}


#if 0
static value read_alignas(parser p) {
    // C11 6.7.5. Valid form of _Alignof is either _Alignas(type-name) or
    // _Alignas(constant-expression).
    expect(p, stringify("("));
    value r = is_type(p, token(p))
        ? pget(read_cast_type(p, env), sym(align))
        : read_intexpr(p, offset);
    expect(p, offset, stringify(")"));
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


static Type read_rectype_def(parser p, index offset, scope env, string kind);

// should* pass class down
Type read_decl_spec(parser p, index offset, scope env, string rsclass) {
    // we are losing this semi - but this is mandatory
    value tok = token(p, offset);
    value k = pget(tok, sym(kind));
    
    if (k == sym(identifier)){
        Type def = get_typedef(p->global, pget(tok, sym(value)));
        if (def) return def;
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
    return 0;
}

static vector read_rectype_fields_sub(parser p, index offset, scope env) {
    tuple r = 0;
    for (;;) {
        if (!is_type(p, token(p, offset))) break; //??

        Type basetype = read_decl_spec(p, offset, env, zero);
        // seems odd
        //        if (pget(basetype, sym(kind)) == sym(struct) && next_token(p, sym(;)))
        // push(r, timm(type, basetype));
        //            continue;
        //        }
        for (;;) {
            buffer name = zero;
            Type fieldtype = read_declarator(p, offset, env, &name, basetype, zero);
            fieldtype = allocate_scope(fieldtype);
            
            //      if (next_token(p, stringify(":")))
            //        set(fieldtype, sym(bitsize), read_bitsize(p, name, fieldtype));
            
            if (next_token(p, offset, stringify(","))) continue;

            // seems like expect could be broadened
            value v;
            if (is_keyword(v = token(p, offset), stringify("}"))) {
                error(p, "missing ';' at the end of field list", v);
            } else
                expect(p, offset, stringify(";"));

            timm("name", name, "type", fieldtype);
        }
    }
    expect(p, offset, stringify("}"));
    return r; // vector of fields
}

static Type read_rectype_def(parser p, index offset, scope env, string kind) {
    buffer tag = read_rectype_tag(p, offset);
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
                (!next_token(p, offset, stringify("{")))?zero:read_rectype_fields_sub(p, offset, env));
}


static value read_bitsize(parser p, index offset, buffer name, Type ty) {
    if (!is_inttype(ty))
        error(p, "non-integer type cannot be a bitfield: %s", ty2s(ty));

    int r = read_intexpr(p, offset);
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

static void read_initializer_elem(parser p, scope env, vector inits, Type ty, boolean designated, index offset) {
    next_token(p, offset, sym(=));
    if (pget(ty, sym(kind)) == sym(array) || pget(ty, sym(kind)) == sym(struct)) {
        read_initializer_list(p, env, inits, ty, designated, offset);
    } else if (next_token(p, offset, stringify("{"))) {
        read_initializer_elem(p, env, inits, ty, true, offset);
        expect(p, offset, stringify("}"));
    } else {
        Node expr = conv(p, read_assignment_expr(p, offset, env));
        // ensure_assignable(ty, pget(expr, sym(type)));
        // push(inits, ast_init(expr, ty));
    }
}

// generic optional
static Type read_func_param(parser p, index offset, scope env, buffer *name, boolean optional) {
    string sclass = 0;
    Type basety = 0;
    
    if (is_type(p, token(p, offset))) {
        basety = read_decl_spec(p, offset, env, sclass);
    } else if (optional) {
        error(p, "type expected, but got", peek());
    }

    Type ty = read_declarator(p, offset, env, name, basety, zero);

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


// Reads an ANSI-style prototyped function parameter list...this is the same, but
// we're deferring many checks
value read_declarator_params(parser p, index offset, scope env, vector types, vector vars)  {
    int length;

    for (;;) {
        tuple tok = token(p, offset);
        if (next_token(p, offset, sym(...))) {
            if (!types) error(p, "at least one parameter is required before \"...\"");
            expect(p, offset, stringify(")"));
            goto construct;
        }
        buffer name;
        Type ty = read_func_param(p, offset, env, &name, true); //typeonly);

        tok = token(p, offset);
        if (is_keyword(tok, stringify(")")))
            goto construct;

        if (!is_keyword(tok, stringify(",")))
            error(p, "stringify(",") expected, but got %s", tok);
    }
 construct:
    return 0; // build vector
}

static Type read_func_param_list(parser p, index offset, scope env, vector paramvars, Type rettype) {
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
        read_declarator_params(p, offset, env, paramtypes, paramvars);
        return make_func_type(rettype, paramtypes);
    }
    if (!paramvars)
        error(p, "invalid function definition");
    vector paramtypes = zero;

    // a param is an object with a type
    //  for (int i = 0; i < vector_length(paramvars); i++)
    // push(paramtypes, pget(p->global, sym(type), sym(int)));
    
    return make_func_type(rettype, paramtypes);
}

static Type read_declarator_tail(parser p, index offset, scope env, Type basety, vector params);


static Type read_declarator_array(parser p, index offset, scope env, Type basety) {
    int len;
    if (next_token(p, offset, stringify("]"))) {
        len = -1;
    } else {
        len = read_intexpr(p, offset);
        expect(p, offset, stringify("}"));
    }
    Type t = read_declarator_tail(p, offset, env, basety, zero);
    if (pget(t, sym(kind)) == sym(func))
        error(p, "array of functions");
    return make_array_type(t, len);
}

static Type read_declarator_tail(parser p, index offset, scope env, Type basety, vector params) {
    if (next_token(p, offset, stringify("{")))
        return read_declarator_array(p, offset, env, basety);
    if (next_token(p, offset, stringify("(")))
        return read_func_param_list(p, offset, env, params, basety);
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
Type read_declarator(parser p, index offset, scope env, buffer *rname, Type basety, vector params) {
    if (next_token(p, offset, stringify("("))) {
        // "(" is either beginning of grouping parentheses or of a function parameter list.
        // If the next token is a type name, a parameter list must follow.
        if (is_type(p, token(p, offset)))
            return read_declarator_func(p, offset, env, basety, params);
        // stub is here to handle some issues with function declarations
        // and grouping
        Type stub = timm("kind", sym(stub)); // stub type
        Type t = read_declarator(p, offset, env, rname, stub, params);
        expect(p, offset, stringify(")"));
        stub = read_declarator_tail(p, offset, env, basety, params);
        return t;
    }
    if (next_token(p, offset, sym(*))) {
        // where is this being denoted?
        return read_declarator(p, offset, env, rname, make_ptr_type(basety), params);
    }
    tuple tok = token(p, offset);
    if (pget(tok, sym(kind)) == sym(identifier)) {
        *rname = pget(tok, sym(sval));
        return read_declarator_tail(p, offset, env, basety, params);
    }
    //    error(p, "identifier, ( or * are expected, but got", tok);
    return read_declarator_tail(p, offset, env, basety, params);
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
    ast_var(p->global, functype, name);
    expect(p, offset, stringify("{"));
    Node r = read_func_body(p, offset, env, functype, name, params);
    return r;
}


vector read_toplevels(parser p, index offset, scope env) {
    u64 scan;
    vector top = 0;
    value v;
    // dont use token here, since we want to assume that eof anywhere else is an error
    while (scan < vector_length(p->lex)) {
        output(pget(p->lex, scan, sym(kind)));
        printf(" toppy\n");
        scan = read_decl(p, offset, env, top, scan);
    }
    return 0;
}

static void define_builtin(parser p, buffer name, Type rettype, vector paramtypes)
{
    ast_var(p->global, make_func_type(rettype, paramtypes), name);
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
    read_toplevels(p, 0, p->global);
    return 0;
}
