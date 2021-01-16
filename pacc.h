
// goes away
#include <runtime.h>
#include <nursery.h>
#include <stdio.h>

// typedef struct scope *scope;
typedef value location;
typedef value scope;
typedef scope Type;
typedef buffer vector;

typedef value tuple;
typedef tuple Node;

#define error(f, ...) ({result r = res(stringify(f), 0); r.success=false; r;})

boolean is_keyword(tuple tok, string c);
value parse(buffer b);
vector lex(buffer b, value keywords);

typedef struct lexer *lexer;
typedef struct parser *parser;
typedef u64 index;

// hoist to tspace
typedef struct result {
    boolean success;
    value v;
    u64 offset;
    value env;
} result;

#define res(__v, __o) ({struct result __k = {true, __v, __o, 0}; __k;})
#define failure(__v, __o) ({struct result __k = {false, __v, __o, 0}; __k;})
#define isfailure(__r) (!((__r).success))

result read_subscript_expr(parser p, index offset, scope env, Node node);
result read_expression(parser p, index offset, scope env);

struct parser {
    vector tokens;
};

#define pget(__e, ...) pget_internal(__e, __VA_ARGS__, INVALID_ADDRESS)
static value pget_internal(void *e, ...)
{
    foreach_arg(e, i) e = get(e, i);
    return e;
}


string make_label();

//result?
result read_declaration(parser p, index offset, scope env);
result read_declarator(parser p, index offset, scope env, Type basety);
result read_decl_spec(parser p, index offset, scope env);


static inline result read_cast_type(parser p, index offset, scope env) {
    // DECL_CAST
    result r = read_decl_spec(p, offset, env);
    return read_declarator(p, r.offset, env, r.v);
}


static Node ast_int_literal(Type ty, value val) {
    return timm("kind", sym(literal), "type", ty, "value", val);
}


// make a property of the type
static inline boolean is_inttype(Type ty) {
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


static inline value vector_peek(vector v)
{
    return zero;
}

#define value_from_u64(_x) ((value)(u64)(_x))
static inline u64 u64_from_value(value v)
{
    if (tagof(v) != tag_small) halt("coercing non-number");
    return (u64)v;
}

static inline Type make_array_type(Type ty, int len) {
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

static inline boolean is_type(scope env, tuple tok)
{
    value k= get(tok, sym(kind));
    value v= get(tok, sym(value));
    if (k == sym(identifier))   return toboolean(pget(env, sym(type), v));
    // lookup type
    if (k != sym(keyword))      return false;
    return false;
}


static result read_statement(parser p, index offset, scope env);

static Node ast_binop(Type ty, string kind, Node left, Node right) {
    return timm("kind", kind, "type", ty, "left", left, "right", right);
}

result read_compound_stmt(parser p, index offset, scope env);


static Node ast_conv(Type totype, Node val) {
    return timm("kind", sym(conv), "type", totype, "operand", val);
}


#define allocate_scope(...) true

result read_assignment_expr(parser p, index offset, scope env);

// we care about the ordering, so a map and a vector..
static inline Type lookup_field(Type t, value s)
{
    return 0;
}

static inline Node ast_var(scope s, Type ty, string name) {
    return timm("kind", sym(variable), "type", ty, "name", name);
}

static inline Type make_ptr_type(Type ty) {
    return timm("kind", sym(ptr), "ptr", ty);
}

static inline Type make_func_type(Type rettype, vector parameters) {
    return timm("kind", sym(func),
                "rettype", rettype,
                "parameters", parameters);
}



static inline boolean is_string(Type ty) {
    return toboolean((pget(ty, sym(kind)) == sym(array)) &&
                     (pget(ty, sym(ptr), sym(kind)) == sym(char)));
}

int read_intexpr(parser p, index offset);

result read_cast_expression(parser p, index offset, scope env);

//static inline boolean next_token(parser p, string kind) {
// this is supposed to be a catchall, but wondering if some non-deterministic
// check might stumble on this

// these are macros so I get some traceability

// eof check
//    outputline(print(v));                     
//    printf("[%s:%d:%s %lld)]\n", __FILE__, __LINE__,__FUNCTION__, __offset); \

#define token(__p, __offset)                        \
    ({value v = get(p->tokens, (value)__offset);    \
    v;})

#define next_token(__p, __offset, __kind) ({            \
    tuple tok = token(p, __offset);                      \
    value res = false;                                   \
    if (is_keyword(tok, __kind)) res = tok;              \
    res;})

#define expect(__p, __offset, __id) {                  \
        tuple __tok = token(__p, __offset);                 \
        if (!is_keyword(__tok, __id))                           \
            return error("'%c' expected, but got", __id, __tok);  \
    }

vector read_decl_init(parser p, index offset, scope env, Type ty);


#define slen(__x) (sizeof(__x)/sizeof(*__x))


static inline value set_internal(value trash, ...)
{
    int total = 0;
    foreach_arg(trash, i) total++;
    value t = allocate_table(total);
    foreach_arg(trash, i) table_insert(t, i, true);
    return t;
}

#define set(...) set_internal(0, __VA_ARGS__, INVALID_ADDRESS)


string emit_expression(value v);
