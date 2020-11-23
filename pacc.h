
// goes away
#include <runtime.h>
#include <stdio.h>

// typedef struct scope *scope;
typedef value location;
typedef value scope;
typedef scope Type;
typedef value vector;

typedef struct lexer *lexer;
typedef value tuple;
typedef tuple Node;

#define error(p, ...)       
boolean is_keyword(tuple tok, string c);
typedef struct lexer *lexer;
tuple get_token(lexer lex);
value parse(buffer b);
lexer create_lex(buffer b);

typedef struct lexer *lexer;
typedef struct parser *parser;
Node conv(parser p, Node node);
Node read_subscript_expr(parser p, scope env, Node node);
Node read_expr(parser p, scope env);
Node conv(parser p, Node node);


#define pget(__e, ...) pget_internal(__e, __VA_ARGS__, INVALID_ADDRESS)
static value pget_internal(void *e, ...)
{
    foreach_arg(e, i) e = get(e, i);
    return e;
}

value token(parser);

string make_label();
void read_decl(parser p, scope env, vector block);
Type read_declarator(parser p, scope env, buffer *rname, Type basety, vector params);
Type read_decl_spec(parser p, scope env, string *rsclass);


static inline Type read_cast_type(parser p, scope env) {
    // DECL_CAST
    return read_declarator(p, env, zero, read_decl_spec(p, env, zero), zero);
}


// mostly for namespace correlation
struct parser {
    lexer lex;
    value readahead;

    scope types; // this is 
    scope file;
    scope global;
};

static inline void unget(parser p, value t)
{
    if (p->readahead) {
        halt("parser double push");
    }
    p->readahead = t;
}

static Node ast_int_literal(parser p, Type ty, value val) {
    return timm("kind", sym(literal), "type", ty, "ival", val);
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
static inline Node ast_string(parser p, scope env, buffer in)
{
    return timm("kind", sym(literal),
                "type", make_array_type(pget(env, sym(type), sym(char)), in->length),
                "value", in);
}

static inline Type get_typedef(scope s, string name) {
    //    Node node = pget(s, sym(types), name);
    return pget(s, sym(types), name);
    //    return (node && (pget(node, sym(kind)) == sym(typedef))) ? pget(node, sym("type")) : zero;
}

static inline boolean is_type(parser p, tuple tok)
{
    scope env = p->global; // xxx 
    value k= get(tok, sym(kind));
    value v= get(tok, sym(value));
    if (k == sym(identifier))   return get_typedef(env, v)?true:false;
    // lookup type
    if (k != sym(keyword))      return false;
    return false;
}


static Node read_stmt(parser p, scope env);

static Node ast_binop(parser p, Type ty, string kind, Node left, Node right) {
    return timm("kind", kind, "type", ty, "left", left, "right", right);
}

Node read_compound_stmt(parser p, scope env);


static Node ast_conv(Type totype, Node val) {
    return timm("kind", sym(conv), "type", totype, "operand", val);
}


#define allocate_scope(...) true

Node read_assignment_expr(parser p, scope env);

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

static inline Type make_func_type(Type rettype, vector paramtypes, boolean has_varargs) {
    return timm("kind", sym(func),
                "rettype", rettype,
                "params", paramtypes,
                "hasva", has_varargs);
}



static inline boolean is_string(Type ty) {
    return toboolean((pget(ty, sym(kind)) == sym(array)) &&
                     (pget(ty, sym(ptr), sym(kind)) == sym(char)));
}

int read_intexpr(parser p);

Node read_cast_expr(parser p, scope env);

void read_initializer_list(parser p,
                           scope env,
                           vector inits,
                           Type ty,
                           boolean designated);



//static inline boolean next_token(parser p, string kind) {
// this is supposed to be a catchall, but wondering if some non-deterministic
// check might stumble on this

// these are macros so I get some traceability

#define token(__p)\
    ({value v;\
    if (__p->readahead) {\
        v = __p->readahead;\
        __p->readahead = 0;\
    } else {\
        v = get_token(__p->lex);\
        if (pget(v, sym(kind)) == sym(eof)) error(__p, "premature end of input");\
    }                                       \
    printf("[%s:%d]", __FUNCTION__, __LINE__);\
    output(print(pget(v, sym(value))));       \
    printf("\n\n");\
    v;})

#define next_token(__p, __kind) ({\
    tuple tok = token(p);\
    value res = false;\
    if (is_keyword(tok, __kind)){\
        res = tok;\
    } else unget(p, tok);\
    res;})

static inline void expect(parser p, string id) {
    tuple tok = token(p);
    if (!is_keyword(tok, id))
        error(p, "'%c' expected, but got", id, tok);
}

