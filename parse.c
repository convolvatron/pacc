#include "pacc.h"

int token_trace = 1;

// cache stringyify

#define read_until(__lex, __offset, __match, __tok )  \
    for (;tok = token(__lex, offset); is_keyword(tok, stringify("__match")); (*offset)++)

void errorf(void *x, char *fmt, ...);


boolean is_keyword(tuple tok, string x)
{
    return toboolean((get(tok, sym(kind)) == sym(keyword)) &&
                     (equals(get(tok, sym(value)), x)));
}


void errorf(void *x, char *format, ...)
{
    halt("parse error");
}

#define get table_get

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

#define allocate_vector(...) false


#if 0
    define_builtin(p, sym(__builtin_return_address), v, voidptr);
    define_builtin(p, sym(__builtin_reg_class),
                   pget(p->global, sym(types), sym(int)),
                   voidptr);
    // parameter list
    define_builtin(p, sym(__builtin_va_arg), vt, allocate_vector(voidptr, voidptr));
    define_builtin(p, sym(__builtin_va_start), vt, allocate_vector(voidptr));
#endif

extern value world();


// just buffer -> graph please
value parse(buffer b)
{
    parser p = malloc(sizeof(struct parser)); // xxx stdlib - we're about to kill this guy anyways
    value root = world();

    value keywords = allocate_nursery(60);
    foreach (k1, v1, get(root, sym(operators))){
        foreach (k2, v2, v1) {
            // soft intern - a union does that, right? i think we disabled overwrite panics
            push_mut_value(keywords, k2);
        }
    }

    p->tokens = lex(b, combine(set_from_nursery(keywords),
                               get(root, sym(tokens))));
    
    u64 scan = 0;
    while (get(p->tokens, (value)scan)) {
        result r = read_declaration(p, scan, root);
        scan = r.offset;
    }

    return 0;
}
