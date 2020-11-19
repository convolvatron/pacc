// Copyright 2012 Rui Ueyama. Released under the MIT license.
#include "pacc.h"
#include <stdlib.h>

void errorf(void *x, char *fmt, ...);

// mostly for namespace correlation
struct parser {
    lexer lex;
    value readahead;

    scope file;
    scope global;
};

// we can tease this out, right?
enum {
      DECL_BODY = 1,
      DECL_PARAM,
      DECL_PARAM_TYPEONLY,
      DECL_CAST,
};

#define NULL ((void *)0)

#define allocate_scope(...) true

boolean is_keyword(tuple tok, string x)
{
    return true;
}

   
static value pget_internal(void *trash, ...)
{
    return 0;
}

#define pget(...) pget_internal(0, __VA_ARGS__, INVALID_ADDRESS)

#define value_from_u64(_x) ((value)(u64)(_x))

void errorf(void *x, char *format, ...)
{
    halt("parse error");
}

static inline void unget(parser p, value t)
{
    if (p->readahead) {
        halt("parser double push");
    }
    p->readahead = t;
}

static tuple token(parser p)
{
    if (p->readahead) {
        value v = p->readahead;
        p->readahead = 0;
        return v;
    }

    // readahead
    return get_token(p->lex);
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

string make_tempname() {
    static int c = 0;
    u8 staging[20];
    return allocate_utf8(staging, sprintf((char *)staging, ".T%d", c++)); // xx - libc
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

static Node ast_uop(string kind, Type ty, Node operand) {
    return timm("kind", kind, "type", ty, "operand", operand);
}

static Node ast_binop(parser p, Type ty, string kind, Node left, Node right) {
    return timm("kind", kind, "type", ty, "left", left, "right", right);
}

// not a type - a literal
static Node ast_inttype(parser p, Type ty, value val) {
    return timm("kind", sym(literal), "type", ty, "ival", val);
}

static Node ast_var(scope s, Type ty, string name) {
    return timm("kind", sym(variable), "type", ty, "name", name);
}

static inline u64 u64_from_value(value v)
{
    if (tagof(v) != tag_small) halt("coercing non-number");
    return (u64)v;
}

#define get table_get

static Type make_array_type(Type ty, int len) {
    int size;
    if (len < 0)
        size = -1;
    else
        size = u64_from_value(get(ty, sym(size))) * len;
    return timm("kind", sym(array),
                "ptr", ty,
                "size", size,
                "len", len);
}

// location - doesn't this just come from lexland?..i guess type
static Node ast_string(parser p, scope env, buffer in)
{
    return timm("kind", sym(literal),
                "type", make_array_type(pget(env, sym(type), sym(char)), in->length),
                "value", in);
}

typedef value vector;

static Node ast_funcptr_call(Node fptr, vector args) {
    return timm("kind", sym(funcptr_call), "type",
                pget(fptr, sym(type), sym(ptr), sym(rettype)),
                "fptr", fptr,
                "args", args);
}

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

static Node ast_conv(Type totype, Node val) {
    return timm("kind", sym(conv), "type", totype, "operand", val);
}

static Node ast_if(Node cond, Node then, Node els) {
    return timm("kind", sym(if), "cond", cond, "then", then, "else", els);
}

static Node ast_ternary(Type ty, Node cond, Node then, Node els)
{
    return timm("kind", sym(ternary), "cond", cond, "then", then, "else", els);
}

static Node ast_return(Node retval)
{
    return timm("kind", sym(return), "retval", retval);
}

static Node ast_compound_stmt(vector stmts)
{
    return timm("kind", sym(compound_stmt), "statments", stmts);
}

static Node ast_struct_ref(Type ty, Node struc, buffer name)
{
    return timm("kind", sym(struct_ref), "type", ty, "struct", struc, "field", name);
}

// should be a node?
static Node ast_goto(buffer label)
{
    return timm("kind", sym(goto), "label", label);
}

// should be a node?
static Node ast_jump(buffer label)
{
    return timm("kind", sym(goto), "label", label);
}

static Node ast_computed_goto(Node expr) {
    return timm("kind", sym(computed_goto), "operand", expr);
}

static Node ast_dest(buffer label) {
    return timm("kind", sym(label), "name", label);
}


static Type make_ptr_type(Type ty) {
    return timm("kind", sym(ptr), "ptr", ty);
}

static Node ast_label_addr(parser p, buffer label) {
    // type void
    return timm("kind", sym(label_addr), "type",
                make_ptr_type(pget(p->global, sym(type), sym(void))),
                "name", label);
}

static Type make_func_type(Type rettype, vector paramtypes, boolean has_varargs) {
    return timm("kind", sym(func),
                "rettype", rettype,
                "params", paramtypes,
                "hasva", has_varargs);
}

/*
 * Predicates and kind checking routines
 */

// make a property of the type
boolean is_inttype(Type ty) {
    value kind = get(ty, sym(kind));
    if (kind == sym(boolean) ||
        kind == sym(char) ||
        kind == sym(short) ||
        kind == sym(int) ||
        kind == sym(long) ||
        kind == sym(llong))
        return true;
    return false;
}


static void ensure_lvalue(Node node) {
    value kind = get(node, sym(kind));
    if (kind == sym(variable) ||
        kind == sym(deref) ||
        kind == sym(struct_ref))
        return;

    error("lvalue expected, but got %s", node2s(node));
}

static void ensure_inttype(Node node) {
    if (!is_inttype(get(node, sym(type))))
        error("integer type expected, but got %s", node2s(node));
}

static void ensure_not_void(Type ty) {
    if (pget(ty, sym(kind)) == sym(void))
        error("void is not allowed");
}

static void expect(parser p, string id) {
    tuple tok = token(p);
    if (!is_keyword(tok, id))
        errort(tok, "'%c' expected, but got %s", id, string_from_token(transient, tok));
}

static Type get_typedef(parser p, string name) {
    Node node = pget(p, sym(types), name);
    return (node && (pget(node, sym(kind)) == sym(typedef))) ? pget(node, sym("type")) : NULL;
}

static boolean is_type(parser p, tuple tok)
{
    value k= get(tok, sym(kind));
    value v= get(tok, sym(value));
    if (k == sym(identifier))
        return get_typedef(p, v)?true:false;
    if (k != sym(keyword))
        return false;
    // all the standard types were pulled in with redefining op
    // typespace
    return false;
}

static boolean next_token(parser p, string kind) {
    tuple tok = token(p);
    if (is_keyword(tok, kind)){
        return true;
    }
    return false;
}

static Node conv(parser p, Node node) {
    Type int_type = pget(p->global, sym(type), sym(int)); // ?
    Type ty = get(node, sym(type));
    string kind = get(ty, sym(kind));
    if (kind == sym(array))
        // c11 6.3.2.1p3: an array of t is converted to a pointer to t.
        return ast_uop(sym(conv), make_ptr_type(get(ty, sym(ptr))), node);
    if (kind == sym(func))
        // c11 6.3.2.1p4: a function designator is converted to a pointer to the function.
        return ast_uop(sym(addr), make_ptr_type(ty), node);
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

static boolean same_arith_type(Type t, Type u) {
    return toboolean(get(t, sym(kind)) == get(u, sym(kind)) && get(t, sym(usig)) == get(u, sym(usig)));
}

static Node wrap(Type t, Node node) {
    if (same_arith_type(t, get(node, "type")))
        return node;
    return ast_uop(sym(conv), t, node);
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

static void ensure_assignable(Type totype, Type fromtype) {
    if ((is_inttype(totype) || get(totype, sym(kind)) == sym(ptr)) &&
        (is_inttype(fromtype) || get(fromtype, sym(kind)) == sym(ptr)))
        return;
    // there was a structural equivalence here - ignore?
    //  if (is_same_struct(totype, fromtype))
    //    return;
    error("incompatible kind: <%s> <%s>", ty2s(totype), ty2s(fromtype));
}

#if 0
// xxx little state machines
static Type read_int_suffix(parser p, buffer b){
    if (buffer_length(b) > 0) {
        if (*(u8 *)buffer_ref(b, 0) == 'u') {
            if (buffer_length(b) > 1) {
                if (*(u8 *)buffer_ref(b, 1) == 'l') {
                    if (buffer_length(b) > 2) {
                        if (*(u8 *)buffer_ref(b, 2) == 'l') {
                            return p->type_ullong;
                        }
                    }
                    return p->type_ulong;
                }
            }
            return p->type_uint;
        }

        if (*(u8 *)buffer_ref(b, 0) == 'l') {
            if (buffer_length(b) > 1) {
                if (*(u8 *)buffer_ref(b, 1) == 'l') {
                    if (buffer_length(b) > 2) {
                        if (*(u8 *)buffer_ref(b, 2) == 'u') {
                            return p->type_ullong;
                        }
                    }
                    return p->type_ulong;
                }
            }
            return p->type_uint;
        }
    }
    return NULL;
}
#endif
static Type read_declarator(parser p, scope env, buffer *rname, Type basety, vector params, int ctx);

static buffer read_rectype_tag(parser p) {
    tuple tok = token(p);
    if (get(tok, sym(kind)) == sym(identifier)) {
        return get(tok, sym(value));
    }
    return NULL;
}

static int read_intexpr(parser p) {
    // xxx - we were doing static evaluation here...pass through
    return 0;
}

static Type read_enum_def(parser p, scope env) {
    buffer tag = NULL;
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
            errort(tok, "declarations of %s does not match", tag);
    }
    if (!is_keyword(tok, stringify("{"))) {
        if (!tag || !pget(env, sym(tags), tag))
            errort(tok, "enum tag %s is not defined", tag);
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
            errort(tok, "identifier expected, but got %s", tok2s(tok));
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
        errort(peek(), "',' or '}' expected, but got %s", tok2s(peek()));
    }
    return pget(p->global, sym(type), sym(int));
}

static Type read_decl_spec(parser p, scope env, string *rsclass);

static Type read_cast_type(parser p, scope env) {
    return read_declarator(p, env, NULL, read_decl_spec(p, env, NULL), NULL, DECL_CAST);    
}

static Node read_assignment_expr(parser p, scope env);

// was read comma_expr
static Node read_expr(parser p, scope env) {
    Node node = read_assignment_expr(p, env);
    while (next_token(p, stringify(","))){
        Node expr = read_assignment_expr(p, env);
        node = ast_binop(p, pget(expr, sym(type)), stringify(","), node, expr);
    }
    return node;
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
#endif
static Type read_rectype_def(parser p, scope env, string kind);

static Type read_decl_spec(parser p, scope env, string *rsclass) {
    string sclass = 0;
    tuple tok = token(p);

    if (!is_type(p, tok))
        errort(tok, "type name expected, but got %s", tok2s(tok));

    Type usertype = NULL;
    string kind = 0;
    string sig = 0;
    string size = 0;
    int align = -1;

    for (;;) {
        tok = token(p);
        value k = pget(tok, sym(kind));
        if (k == sym(eof))
            error("premature end of input");
        if (kind == 0 && k == sym(identifier) && !usertype) {
            Type def = get_typedef(p, pget(tok, sym(value)));
            if (def) {
                if (usertype) goto err;
                usertype = def;
                goto errcheck;
            }
        }
        if (k != sym(keyword)) {
            break;
        }

        value id = pget(token, sym(id));
        if ((id != sym(const)) &&
            (id != sym(volatile)) &&
            (id != sym(inline)) &&
            (id != sym(noreturn))) {
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
            if ((id == sym(void))  ||
                (id == sym(boolean)) ||
                (id == sym(char)) ||
                (id == sym(int)))  {if (kind) goto err; kind = id; }
            if (id == sym(signed))  {if (sig) goto err; sig = sym(signed); }
            if (id == sym(unsigned)) {if (sig) goto err; sig = sym(unsigned); }
            if (id == sym(short))  {if (size) goto err; size = sym(short);}

            if ((id == sym(struct)) || (id == sym(union)))
                {if (usertype) goto err; usertype = read_rectype_def(p, env, id); }

            if (id == sym(enum))   {if (usertype) goto err; usertype = read_enum_def(p, env); }
#if 0
            if (id == sym(alignas)) {
                value nval = read_alignas(p);
                // C11 6.7.5p6: alignas(0) should have no effect.
                if (nval != 0) {
                    if (align == -1 || val < align)
                        align = nval;
                }
            }
#endif
            if (id == sym(long)) {
                if (size == 0) size = sym(long);
                else if (size == sym(long)) size = sym(llong);
                else goto err;
            }
            if (id == sym(typeof)) {
                if (usertype) goto err;
                usertype = read_typeof(p, env);
            }
            goto done;
        }
    errcheck:

        if (kind == sym(boolean) && (size != 0 && sig != 0))
            goto err;
        if (size == sym(short) && (kind != 0 && kind != sym(int)))
            goto err;
        if (size == sym(long) && (kind != 0 && kind != sym(int)))
            goto err;
        if (sig != 0 && (kind == sym(void)))
            goto err;
        if (usertype && (kind != 0 || size != 0 || sig != 0))
            goto err;
    }
 done:
    if (rsclass)
        *rsclass = sclass;
    if (usertype)
        return usertype;
    if (align != -1 && !is_poweroftwo(align))
        errort(tok, "alignment must be power of 2, but got %d", align);
    Type ty;
    // get the canonical copy
    ty = timm("kind", kind, "sig", sig);
    error("internal error: kind: %d, size: %d", kind, size);
    //  if (align != -1)
    //    ty->align = align;
    return ty;
 err:
    errort(tok, "type mismatch: %s", tok2s(tok));
    return 0;
}

static vector read_rectype_fields_sub(parser p, scope env) {
    tuple r = 0;
    for (;;) {
        if (!is_type(p, token(p))) break; //??

        Type basetype = read_decl_spec(p, env, NULL);
        if (pget(basetype, sym(kind)) == sym(struct) && next_token(p, sym(;))) {
            // push(r, timm(type, basetype));
            continue;
        }
        for (;;) {
            buffer name = NULL;
            Type fieldtype = read_declarator(p, env, &name, basetype, NULL, DECL_PARAM_TYPEONLY);
            ensure_not_void(fieldtype);
            fieldtype = allocate_scope(fieldtype);
            //      if (next_token(p, stringify(":")))
            //        set(fieldtype, sym(bitsize), read_bitsize(p, name, fieldtype));
            if (next_token(p, stringify(","))) continue;
            if (is_keyword(token(p), stringify("}"))) {
                // location
                errorf(p, "missing ';' at the end of field list"); // xx -location
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
            error("declarations of %s does not match", tag);
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
        error("non-integer type cannot be a bitfield: %s", ty2s(ty));

    int r = read_intexpr(p);
    int maxsize = u64_from_value(pget(ty, value_from_u64(sym(kind) == sym(boolean) ? 1 :
                                                         u64_from_value(pget(ty, sym(size))) * 8)));
    if (r < 0 || maxsize < r)
        errort(tok, "invalid bitfield size for %s: %d", ty2s(ty), r);
    if (r == 0 && name != NULL)
        errort(tok, "zero-width bitfield needs to be unnamed: %s", name);
    return value_from_u64(r);
}

static Node read_unary_expr(parser p, scope env);

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

#define vector_length(_x) 1

static vector read_func_args(parser p, scope env,vector params) {
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
        ensure_assignable(paramtype, ty);
        if (pget(paramtype, sym(kind)) != pget(arg, sym(type), sym(kind)))
            arg = ast_conv(paramtype, arg);

        //args = push(args, arg);

        tuple tok = token(p);
        if (is_keyword(tok, stringify(")"))) break;
        if (!is_keyword(tok, stringify(",")))
            errort(tok, "unexpected token: '%s'", tok2s(tok));
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
    return ast_funcptr_call(fp, args);
}

// fold 
static Node ast_funcdesg(Type ty, string fname)
{
    return timm("kind", sym(funcdesg), "tupe", ty, "fname", fname);
}

static Node read_var_or_func(parser p, scope env, buffer name) {
    Node v = pget(env, name);
    if (!v) {
        tuple tok = token(p);
        if (!is_keyword(tok, stringify("(")))
            errort(tok, "undefined variable: %s", name);
        Type ty = make_func_type(pget(p->global, sym(type), sym(int)),
                                 0, false);
        // warnt(tok, "assume returning int: %s()", name);
        return ast_funcdesg(ty, name);
    }
    // so..funcdesg is really just a variable of function type?
    if (pget(v, sym(type), sym(kind)) == sym(func))
        return ast_funcdesg(pget(v, sym(type)), name);
    return v;
}

static string get_compound_assign_op(tuple tok) {
    if (pget(tok, sym(kind)) != sym(keyword))
        return 0;
    return(pget(tok, sym(id)));
}

static Node read_compound_stmt(parser p, scope env);

static value vector_peek(vector v)
{
    return zero;
}

static Node read_stmt(parser p, scope env);

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
    // if (!tok) return NULL;
    value k = pget(tok, sym(kind));
    value v = pget(tok, sym(value));
    if (is_keyword(tok, stringify("("))) {
        if (next_token(p, stringify("[")))
            return read_stmt_expr(p, env);
        Node r = read_expr(p, env);
        expect(p, stringify(")"));
        return r;
    }
    if (k == sym(keyword)) return NULL;
    if (k == sym(identifier)) return read_var_or_func(p, env, v);
    //  if (tok->kind == sym(number))
    //    return read_int(p, tok);
    if (k == sym(char))  return ast_inttype(p, pget(p->global, sym(type), sym(char)), v);
    if (k == sym(string)) return ast_string(p, env, v);
    error("internal error: unknown token kind: %d", k);
    return 0;
}


static Node read_subscript_expr(parser p, scope env, Node node) {
    tuple tok = token(p);
    Node sub = read_expr(p, env);
    if (!sub) errort(tok, "subscript expected");
    expect(p, stringify("]"));
    Node t = binop(p, sym(+), conv(p, node), conv(p, sub));
    return ast_uop(sym(deref), pget(t, sym(type), sym(ptr)), t);
}

static Node read_struct_field(parser p, Node struc);

static Node read_postfix_expr_tail(parser p, scope env, Node node) {
    if (!node) return NULL;
    for (;;) {
        if (next_token(p, stringify("("))) {
            node = conv(p, node);
            Type t = pget(node, sym(type));
            if (pget(node, sym(kind)) != sym(ptr) || pget(t, sym(ptr), sym(kind) != sym(func)))
                errort(tok, "function expected, but got %s", node2s(node));
            node = read_funcall(p, env, node);
            continue;
        }
        if (next_token(p, stringify("["))) {
            node = read_subscript_expr(p, env, node);
            continue;
        }
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
            ensure_lvalue(node);
            string op = is_keyword(tok, sym(inc)) ? sym(post_inc) : sym(post_dec);
            return ast_uop(op, pget(node, sym(type)), node);
        }
        return node;
    }
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
        errort(tok, "label name expected after &&, but got %s", tok2s(tok2));
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
        errort(tok, "pointer type expected, but got %s", node2s(operand));
    if (pget(ot, sym(ptr), sym(kind)) == sym(func))
        return operand;
    return ast_uop(sym(deref), pget(ot, sym(ptr)), operand);
}

static Node read_unary_minus(parser p, scope env) {
    Node expr = read_cast_expr(p, env);
    ensure_inttype(expr);
    return binop(p, sym(-), conv(p, ast_inttype(p, pget(expr, sym(type)), 0)), conv(p, expr));
}

static Node read_unary_bitnot(parser p, scope env, tuple tok) {
    Node expr = read_cast_expr(p, env);
    expr = conv(p, expr);
    Type et = pget(expr, sym(type));
    if (!is_inttype(et))
        errort(tok, "invalid use of ~: %s", node2s(expr));
    return ast_uop(sym(~), et, expr);
}

static Node read_unary_lognot(parser p, scope env) {
    Node operand = read_cast_expr(p, env);
    operand = conv(p, operand);
    return ast_uop(sym(!), pget(p->global, sym(type), sym(int)), operand);
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

static boolean is_string(Type ty) {
    return toboolean((pget(ty, sym(kind)) == sym(array)) && (pget(ty, sym(ptr), sym(kind)) == sym(char)));
}


static void assign_string(parser p, vector inits, Type ty, buffer s) {
}
#if 0
if (ty->len == -1)
    ty->len = ty->size = buffer_length(s);
int i = 0;
for (; i < ty->len && *p; i++)
    push(inits, ast_init(ast_inttype(p, p->type_char, *p++), p->type_char, off + i));
for (; i < ty->len; i++)
    push(inits, ast_init(ast_inttype(p, p->type_char, 0), p->type_char, off + i));
}
#endif

static boolean maybe_read_brace(parser p) {
    return next_token(p, stringify("{"))?true:false;
}

static void maybe_skip_comma(parser p) {
    next_token(p, stringify(","));
}

static void skip_to_brace(parser p, scope env) {
    for (;;) {
        if (next_token(p, stringify("}")))
            return;
        if (next_token(p, sym(.))) {
            expect(p, sym(=));
        }
        Node ignore = read_assignment_expr(p, env);
        if (!ignore)
            return;
        errorf("excessive initializer", ignore);
        maybe_skip_comma(p);
    }
}

static void read_initializer_elem(parser p, scope env, vector inits, Type ty, boolean designated);

static void read_array_initializer(parser p, scope env, vector inits, Type ty, boolean *designated) {
    boolean has_brace = maybe_read_brace(p);
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
            //        errort(tok, "array designator exceeds array bounds: %d", idx);
            i = idx;
            expect(p, stringify("}"));
            *designated = true;
        }
        read_initializer_elem(p, env, inits, pget(ty, sym(ptr)), designated);
        maybe_skip_comma(p);
        *designated = false;
    }
    if (has_brace)
        skip_to_brace(p, env);
    // finish:
    // is this is a default int thing?
    //    if (!pget(ty, sym(len))) {
    //        set(ty, sym(len), value_from_u64(i));
    //        set(ty, sym(size), value_from_u64(u64_from_value(elemsize) * i));
    //    }
}

static void read_struct_initializer(parser p, scope env, vector inits, Type ty, boolean designated) {
    boolean has_brace = maybe_read_brace(p);
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
                errort(tok, "malformed desginated initializer: %s", tok2s(tok));
            fieldtype = lookup_field(ty, pget(tok, sym(value)));
            if (!fieldtype)
                errort(tok, "field does not exist: %s", tok2s(tok));
            designated = true;
        } else {
            fieldtype = lookup_index(ty, i++);
        }
        // off? really?
        read_initializer_elem(p, env, inits, fieldtype, designated);
        maybe_skip_comma(p);
        designated = false;
        if (pget(ty, sym(kind)) != sym(struct))
            break;
    }
    if (has_brace) skip_to_brace(p, env);
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
        if (is_keyword(tok, stringify("{")) && (pget(p, sym(kind)) == sym(string))) {
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
        ensure_inttype(node);
        ensure_inttype(right);
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
        return ast_ternary(r, cond, (then ? wrap(r, then) : NULL), wrap(r, els));
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

static Node read_struct_field(parser p, Node struc) {
    // or union?
    Type ty = pget(struc, sym(type));
    if (pget(ty, sym(kind)) != sym(struct))
        error("struct expected, but got %s", node2s(struc));
    tuple name = token(p);
    if (pget(name, sym(kind)) != sym(identifier))
        error("field name expected, but got %s", tok2s(name));
    Type field = lookup_field(ty, pget(name, sym(value)));
    if (!field)
        error("struct has no such field: %s", tok2s(name));
    return ast_struct_ref(field, struc, pget(name, sym(value)));
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
        ensure_assignable(ty, pget(expr, sym(type)));
        // push(inits, ast_init(expr, ty));
    }
}


static Type read_func_param(parser p, scope env, buffer *name, boolean optional) {
    string sclass = 0;
    Type basety = pget(p->global, sym(type), sym(int));
    if (is_type(p, token(p))) {
        basety = read_decl_spec(p, env, &sclass);
    } else if (optional) {
        errort(peek(), "type expected, but got %s", tok2s(peek()));
    }
    Type ty = read_declarator(p, env, name, basety, NULL, optional ? DECL_PARAM_TYPEONLY : DECL_PARAM);

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
static void read_declarator_params(parser p, scope env, vector types, vector vars, boolean *ellipsis) {
    boolean typeonly = toboolean(!vars);
    *ellipsis = false;
    for (;;) {
        tuple tok = token(p);
        if (next_token(p, sym(...))) {
            if (!types) errort(tok, "at least one parameter is required before \"...\"");
            expect(p, stringify(")"));
            *ellipsis = true;
            return;
        }
        buffer name;
        Type ty = read_func_param(p, env, &name, typeonly);
        ensure_not_void(ty);
        // push(types, ty);
        //        if (!typeonly)
        //            push(vars, ast_var(env, ty, name));
        tok = token(p);
        if (is_keyword(tok, stringify(")")))
            return;
        if (!is_keyword(tok, stringify(",")))
            errort(tok, "stringify(",") expected, but got %s", tok2s(tok));
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
        errort(tok2, "at least one parameter is required before \"...\"");
    if (is_type(p, token(p))) {
        boolean ellipsis;
        vector paramtypes = zero;
        read_declarator_params(p, env, paramtypes, paramvars, &ellipsis);
        return make_func_type(rettype, paramtypes, ellipsis);
    }
    if (!paramvars)
        errort(tok, "invalid function definition");
    vector paramtypes = zero;
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
    Type t = read_declarator_tail(p, env, basety, NULL);
    if (pget(t, sym(kind)) == sym(func))
        errort(tok, "array of functions");
    return make_array_type(t, len);
}

static Type read_declarator_func(parser p, scope env, Type basety, vector param) {
    if (pget(basety, sym(kind)) == sym(func))
        error("function returning a function");
    if (pget(basety, sym(kind)) == sym(array))
        error("function returning an array");
    return read_func_param_list(p, env, param, basety);
}

static Type read_declarator_tail(parser p, scope env, Type basety, vector params) {
    if (next_token(p, stringify("{")))
        return read_declarator_array(p, env, basety);
    if (next_token(p, stringify("(")))
        return read_declarator_func(p, env, basety, params);
    return basety;
}

static void skip_type_qualifiers(parser p) {
    while (next_token(p, sym(const)) || next_token(p, sym(volatile)) || next_token(p, sym(RESTRICT)));
}

// C11 6.7.6: Declarators
static Type read_declarator(parser p, scope env, buffer *rname, Type basety, vector params, int ctx) {
    if (next_token(p, stringify("("))) {
        // stringify("(") is either beginning of grouping parentheses or of a function parameter list.
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
        Type t = read_declarator(p, env, rname, stub, params, ctx);
        expect(p, stringify(")"));
        stub = read_declarator_tail(p, env, basety, params);
        return t;
    }
    if (next_token(p, sym(*))) {
        skip_type_qualifiers(p);
        return read_declarator(p, env, rname, make_ptr_type(basety), params, ctx);
    }
    tuple tok = token(p);
    if (pget(tok, sym(kind)) == sym(identifier)) {
        if (ctx == DECL_CAST)
            errort(tok, "identifier is not expected, but got %s", tok2s(tok));
        *rname = pget(tok, sym(sval));
        return read_declarator_tail(p, env, basety, params);
    }
    if (ctx == DECL_BODY || ctx == DECL_PARAM)
        errort(tok, "identifier, ( or * are expected, but got %s", tok2s(tok));
    //  unget_token(tok);
    return read_declarator_tail(p, env, basety, params);
}

static void read_static_local_var(parser p, scope env, Type ty, buffer name) {
    // there was some kind of uniquification...mixing up the
    // lexical and storage scopes here
    Node var = ast_var(env, ty, name);
    vector init = NULL;
    if (next_token(p, sym(=))) {
        // this had a push scope - new var is bound in initializer?
        init = read_decl_init(p, env, ty);
    }
    // set(env, name, ast_decl(var, init));
    //  push(p->toplevels, ast_decl(var, init));
}

// this has to be jiggered to include function declarations - there was
// a scan-ahead-and-unget-tokens loop before
static void read_decl(parser p, scope env, vector block) {
    string sclass = 0;
    int isstatic = 0, isglobal = 0;
    Type basetype = read_decl_spec(p, env, &sclass);
    if (next_token(p, stringify(";")))
        return;
    for (;;) {
        buffer name = NULL;
        Type ty = read_declarator(p, env, &name, basetype, NULL, DECL_BODY); 
        // why do we care ..storage scope
        //        if (sclass == sym(static)) {
        //            set(ty, sym(isstatic), sym(true));
        //        }
        // xxx - are all typedefs always global?
        if (sclass == sym(typedef)) {
            Node r = timm("kind", sym(typedef), "type", ty);
            // set(p->global, name, r);
        } else if (isstatic && !isglobal) {
            ensure_not_void(ty);
            // why is this syntactically special?
            read_static_local_var(p, env, ty, name);
        } else {
            ensure_not_void(ty);
            Node var = ast_var((isglobal ? p->global : env), ty, name);
            if (next_token(p, sym(=))) {
                //push(block, ast_decl(var, read_decl_init(p, env, ty)));
            } else if (sclass != sym(extern) && pget(ty, sym(kind)) != sym(func)) {
                //push(block, ast_decl(var, NULL));
            }
        }
        if (next_token(p, stringify(";"))) return;
        
        if (!next_token(p, stringify(":")))
            errort(peek(), "';' or ',' are expected, but got %s", tok2s(peek()));
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

// this is another big scan ahead machine
static void skip_parentheses(parser p) {
    for (;;) {
        tuple tok = token(p);
        if (pget(tok, sym(kind)) == sym(eof))
            error("premature end of input");
        // push(buf, tok);
        if (is_keyword(tok, stringify(")")))
            return;
        if (is_keyword(tok, stringify("(")))
            skip_parentheses(p);
    }
}

// is_funcdef returns true if we are at beginning of a function definition.
// The basic idea is that if we see '{' or a type keyword after a closing
// parenthesis of a function parameter list, we were reading a function
// definition. (Usually '{' comes after a closing parenthesis.
// A type keyword is allowed for K&R-style function definitions.)

#if 0
// better just nondeterminacy
static boolean is_funcdef(parser p) {
    // vector buf = 0; -- we can scan ahead here, but lets not do this pushy thing
    boolean r = false;
    for (;;) {
        tuple tok = token(p);
        value k = pget(token, sym(kind));
        // push(buf, tok);
        if (k == sym(eof))
            error("premature end of input");
        if (is_keyword(tok, stringify(";")))
            break;
        if (is_type(p, tok))
            continue;
        if (is_keyword(tok, stringify("("))) {
            skip_parentheses(p);
            continue;
        }
        if (k != sym(identifier))
            continue;
        if (!is_keyword(tok, stringify("(")))
            continue;
        // 
        // push(buf, token(p));
        // skip_parentheses(p, buf);
        r = toboolean(is_keyword(token(p), stringify("{")) || is_type(p, token(p)));
        break;
    }
    //  while (vector_len(buf) > 0)
    //    unget_token(vector_pop(buf));
    return r;
}

static void backfill_labels(parser p) {
    for (int i = 0; i < vector_length(pget(env, sym(gotos))); i++) {
        Node src = vector_pget(p->gotos, i);
        buffer label = pget(src, sym(label));
        Node dst = pget(env, sym(labels), label);
        if (!dst)
            error("stray %s: %s", src->kind == sym(goto) ? "goto" : "unary &&", label);
        if (dst->newlabel)
            src->newlabel = dst->newlabel;
        else
            src->newlabel = dst->newlabel = make_label();
    }
}
#endif

static Node read_funcdef(parser p, scope env) {
    string sclass = 0;
    Type basetype = read_decl_spec(p, env, &sclass);
    //    push_scope(p);
    buffer name;
    vector params = 0;
    Type functype = read_declarator(p, env, &name, basetype, params, DECL_BODY);
    // why do we care? make sure there is a file scope
    //  set(functype->isstatic = (sclass == sym(static));
    ast_var(p->global, functype, name);
    expect(p, stringify("{"));
    Node r = read_func_body(p, env, functype, name, params);
    //  backfill_labels(p);
    return r;
}

static Node read_if_stmt(parser p, scope env) {
    expect(p, stringify("("));
    Node cond = read_expr(p, env);
    expect(p, stringify(")"));
    Node then = read_stmt(p, env);
    if (!next_token(p, sym(else)))
        return ast_if(cond, then, NULL);
    Node els = read_stmt(p, env);
    return ast_if(cond, then, els);
}

static void read_decl_or_stmt(parser p, scope env, vector list) {
    tuple tok = token(p);
    if (pget(tok, sym(kind)) == sym(eof))
        error("premature end of input");
    // mark_location();
    if (is_type(p, tok)) {
        read_decl(p, env, list);
    } else {
        Node stmt = read_stmt(p, env);
        //   if (stmt)
        //  push(list, stmt);
    }
}

static Node read_opt_decl_or_stmt(parser p, scope env) {
    if (next_token(p, stringify(";")))
        return NULL;
    vector list = 0;
    read_decl_or_stmt(p, env, list);
    return ast_compound_stmt(list);
}



static Node read_for_stmt(parser p, scope env) {
    expect(p, stringify("("));
    buffer beg = make_label();
    buffer mid = make_label();
    buffer end = make_label();
    // push_scope(p);
    Node init = read_opt_decl_or_stmt(p, env);
    Node cond = read_expr(p, env);
    expect(p, stringify(";"));
    Node step = read_expr(p, env);
    expect(p, stringify(")"));
    Node body = read_stmt(p, env);

    return ast_compound_stmt(timm("init", init,
                                  "cond", cond?ast_if(cond, NULL, ast_jump(end)):0,
                                  "step", step,
                                  "begin", ast_jump(beg),
                                  "end", ast_dest(end)));
}


#define allocate_vector(...) true

static Node read_while_stmt(parser p, scope env) {
    expect(p, stringify("("));
    Node cond = read_expr(p, env);
    expect(p, stringify(")"));

    buffer beg = make_label();
    buffer end = make_label();
    // push_scope(p);
    //   SET_JUMP_LABELS(beg, end);
    Node body = read_stmt(p, env);


    // why not timm here?
    return ast_compound_stmt(timm(// "end", ast_dest(begin), // ? 
                                  "cond", ast_if(cond, body, ast_jump(end)),
                                  "begin", ast_jump(beg),
                                  "end", ast_dest(end)));

}


static Node read_do_stmt(parser p, scope env)
{
    buffer beg = make_label();
    buffer end = make_label();
    Node body = read_stmt(p, env);
    
    tuple tok = token(p);
    if (!is_keyword(tok, sym(while)))
        errort(tok, "'while' is expected, but got %s", tok2s(tok));
    expect(p, stringify("("));
    Node cond = read_expr(p, env);
    expect(p, stringify(")"));
    expect(p, stringify(";"));
    
    // we can set body to zero and it will...do the right thing!
    return ast_compound_stmt(timm("begin", ast_dest(beg),
                                  "if", ast_if(cond, ast_jump(beg), NULL),
                                  "body", body,
                                  "end", ast_dest(end)));
}

static Node make_switch_jump(parser p, Node var, tuple c) {
    Node cond;
    Type int_type = pget(p, sym(type), sym(int)); // ?
    if (pget(c, sym(beg)) == pget(c, sym(end))) {
        Type type_int = pget(p->global, sym(type), sym(int));
        cond = ast_binop(p, type_int, sym(=), var, ast_inttype(p, int_type,
                                                               pget(c, sym(begin))));
        //                                                               value_from_u64(c->beg)));
    } else {
        // [GNU] case i ... j is compiled to if (i <= cond && cond <= j) goto <label>.
        Node x = ast_binop(p, int_type, sym(>=), ast_inttype(p, int_type, pget(c, sym(beg))), var);
        Node y = ast_binop(p, int_type, sym(<=), var, ast_inttype(p, int_type, pget(c, sym(end))));
        cond = ast_binop(p, int_type, sym(logand), x, y);
    }
    // not really
    return ast_if(cond, ast_jump(pget(c, sym(name))), NULL);
}

#define foreach(_k, _v, _t)                     \
    for (;;)

static Node read_switch_stmt(parser p, scope env)
{
    expect(p, stringify("("));
    Node expr = conv(p, read_expr(p, env));
    ensure_inttype(expr);
    expect(p, stringify(")"));

    buffer end = make_label();
    // push_scope(p);
    Node body = read_stmt(p, env);

        
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

static Node read_label_tail(parser p, scope env, Node label) {
    Node stmt = read_stmt(p, env);
    vector v = 0;
    // push(v, label);
    //    if (stmt)
    //        push(v, stmt);
    return ast_compound_stmt(v);
}

// recurse and larvate
static Node read_case_label(parser p, scope env, tuple tok) {
    vector cases = pget(env, sym(cases));
    if (!cases) errort(tok, "stray case label");
    buffer label = make_label();
    int beg = read_intexpr(p);
    if (next_token(p, sym(...))) {
        int end = read_intexpr(p);
        expect(p, stringify(":"));
        if (beg > end)
            errort(tok, "case region is not in correct order: %d ... %d", beg, end);
        // push(cases, make_case(beg, end, label));
    } else {
        expect(p, stringify(":"));
        // push(cases, make_case(beg, beg, label));
    }
    // inline - this seems..broken anyways - do it on insert!
    //   check_case_duplicates(cases);
    return read_label_tail(p, env, ast_dest(label));
}

static Node read_default_label(parser p, scope env, tuple tok) {
    expect(p, stringify(":"));
    if (pget(p, sym(defaultcase)))
        errort(tok, "duplicate default");
    value lab = make_label();
    return read_label_tail(p,
                           allocate_scope(env, sym(defaultcase), lab), // lifetime
                           ast_dest(lab));
}

static Node read_break_stmt(parser p, scope env, tuple tok) {
    expect(p, stringify(";"));
    value b;
    if (!(b =pget(env, sym(targets), sym(break))))
        errort(tok, "stray break statement");
    return ast_jump(b);
}

static Node read_continue_stmt(parser p, scope env, tuple tok) {
    expect(p, stringify(";"));
    value lc;
    if (!(lc =pget(env, sym(targets), sym(continue))))
        errort(tok, "stray continue statement");
    return ast_jump(lc);
}

static Node read_return_stmt(parser p, scope env) {
    Node retval = read_expr(p, env);
    expect(p, stringify(";"));
    if (retval)
        return ast_return(ast_conv(pget(env, sym(__return_type)), retval));
    return ast_return(NULL);
}

static Node read_goto_stmt(parser p, scope env) {
    if (next_token(p, sym(*))) {
        // [GNU] computed goto. "goto *p" jumps to the address pointed by p.

        Node expr = read_cast_expr(p, env);
        if (pget(expr, sym(type), sym(kind)) != sym(ptr))
            errort(tok, "pointer expected for computed goto, but got %s", node2s(expr));
        return ast_computed_goto(expr);
    }
    tuple tok = token(p);
    if (!tok || (pget(tok, sym(kind)) != sym(identifier)))
        errort(tok, "identifier expected, but got", tok);
    expect(p, stringify(";"));
    Node r = ast_goto(pget(tok, sym(value)));
    // why...am I keep track of the gotos? for fixup? - yes
    // push(pget(p, sym(gotos)), r);
    return r;
}

static Node read_label(parser p, scope env, tuple tok)
{
    buffer label = pget(tok, sym(sval));
    if (pget(env, sym(labels), label))
        errort(tok, "duplicate label: %s", tok2s(tok));
    Node r = timm("kind", sym(label), "name", label);
    //    set(pget(p, sym(labels)), sym(label), r);
    return read_label_tail(p, env, r);
}

static Node read_stmt(parser p, scope env) {
    tuple tok = token(p);
    value id = pget(tok, sym(id));
    value k = pget(tok, sym(kind));
    if (k == sym(keyword)) {
        if (id == stringify("{")) read_compound_stmt(p, env);
        if (id == sym(if)) read_if_stmt(p, env);
        if (id == sym(for)) read_for_stmt(p, env);
        if (id == sym(while))  return read_while_stmt(p, env);
        if (id == sym(do))    return read_do_stmt(p, env);
        if (id == sym(return))  return read_return_stmt(p, env);
        if (id == sym(switch))  return read_switch_stmt(p, env);
        if (id == sym(case))   return read_case_label(p, env, tok);
        if (id == sym(default)) return read_default_label(p, env,  tok);
        if (id == sym(break))  return read_break_stmt(p, env, tok);
        if (id == sym(continue)) return read_continue_stmt(p, env, tok);
        if (id == sym(goto))   return read_goto_stmt(p, env);
    }
    if ((k == sym(identifier)) && next_token(p, stringify(":")))
        return read_label(p, env, tok);

    //  unget_token(tok);
    Node r = read_expr(p, env);
    expect(p, stringify(";"));
    return r;
}

static Node read_compound_stmt(parser p, scope env)
{
    // push_scope(p);
    vector list = 0;
    for (;;) {
        if (next_token(p, stringify("}")))
            break;
        read_decl_or_stmt(p, env, list);
    }
    return ast_compound_stmt(list);
}


vector read_toplevels(parser p, scope env) {
    vector top = 0;
    token(p);
    value v;
    while ((v = pget(token(p), sym(kind))) != sym(eof))
        read_decl(p, env, top);
    return 0;
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

static void define_builtin(parser p, buffer name, Type rettype, vector paramtypes) {
    ast_var(p->global, make_func_type(rettype, paramtypes, false), name);
}

void make_numeric_type(scope s, string name, int length, boolean issigned)
{

}

#define vector(...)

// just buffer -> graph please
value parse(lexer lex)
{
    parser p = malloc(sizeof(struct parser)); // xxx stdlib
    // chained set
    Type vt = timm("kind", sym(void));
    // set(pget(p->global, sym(types)), sym(void), vt);
    Type v = make_ptr_type(vt);

    // present as a single batch
    make_numeric_type(p->global, sym(boolean), 1, false);
    make_numeric_type(p->global, sym(char), 8, true);
    make_numeric_type(p->global, sym(short), 16, true);
    make_numeric_type(p->global, sym(int), 32, true);
    make_numeric_type(p->global, sym(long), 64, true);
    make_numeric_type(p->global, sym(llong), 128, true);
    make_numeric_type(p->global, sym(uchar), 8, false);
    make_numeric_type(p->global, sym(ushort), 16, false);
    make_numeric_type(p->global, sym(uint), 32, false);
    make_numeric_type(p->global, sym(ulong), 64, false);
    make_numeric_type(p->global, sym(ullong), 128, false);

    value voidptr;

    define_builtin(p, sym(__builtin_return_address), v, voidptr);
    define_builtin(p, sym(__builtin_reg_class),
                   pget(p, sym(types), sym(int)),
                   voidptr);
    // parameter list
    define_builtin(p, sym(__builtin_va_arg), vt, allocate_vector(voidptr, voidptr));
    define_builtin(p, sym(__builtin_va_start), vt, allocate_vector(voidptr));
    value env; 
    p->global = env = allocate_scope(0);
    read_toplevels(p, env);
    return 0;
}
