
// goes away
#include <runtime.h>
#include <stdio.h>

// typedef struct scope *scope;
typedef value location;
typedef value scope;
typedef scope Type;
typedef buffer vector;

typedef value tuple;
typedef tuple Node;

#define error(p, ...)       
boolean is_keyword(tuple tok, string c);
value parse(buffer b);
vector lex(buffer b);

typedef struct lexer *lexer;
typedef struct parser *parser;
typedef u64 index;

// sadness..i guess we could put some error in here too, or. ... hoist it to
// tspace
typedef struct result {
    value v;
    u64 offset;
} result;

#define res(__v, __o) ({struct result __k = {__v, __o}; __k;})

// dont..really know that we want this here? 
result conv(scope env, result node);

result read_subscript_expr(parser p, index offset, scope env, Node node);
result read_expr(parser p, index offset, scope env);

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

u64 read_decl(parser p, index offset, scope env, vector block);
result read_declarator(parser p, index offset, scope env, buffer *rname, Type basety);
result read_decl_spec(parser p, index offset, scope env, string rsclass);


static inline result read_cast_type(parser p, index offset, scope env) {
    // DECL_CAST
    result r = read_decl_spec(p, offset, env, zero);
    return read_declarator(p, r.offset, env, zero, r.v);
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

// location - doesn't this just come from lexland?..i guess type
static inline Node ast_string(scope env, buffer in)
{
    return timm("kind", sym(literal),
                "type", make_array_type(pget(env, sym(type), sym(char)), in->length),
                "value", in);
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

Node read_cast_expr(parser p, index offset, scope env);


//static inline boolean next_token(parser p, string kind) {
// this is supposed to be a catchall, but wondering if some non-deterministic
// check might stumble on this

// these are macros so I get some traceability

// eof check
#define token(__p, __offset)                        \
    ({value v = get(p->tokens, (value)__offset);    \
      printf("[%s:%d:%p]", __FUNCTION__, __LINE__,v);  \
    output(print(v));\
    printf("\n");                                   \
    v;})

#define next_token(__p, __offset, __kind) ({            \
    tuple tok = token(p, __offset);                      \
    value res = false;                                   \
    if (is_keyword(tok, __kind)) res = tok;              \
    res;})

static inline void expect(parser p, u64 offset, string id) {
    tuple tok = token(p, offset);
    if (!is_keyword(tok, id))
        error(p, "'%c' expected, but got", id, tok);
}

vector read_decl_init(parser p, index offset, scope env, Type ty);


