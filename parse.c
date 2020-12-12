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

vector read_toplevels(parser p, index offset, scope env) {
    u64 scan;
    vector top = 0;
    value v;
    while (scan < (p->tokens->length/sizeof(value))) {
        scan = read_decl(p, scan, env, top);
    }
    return 0;
}

struct numeric {value name; int length; boolean has_sign;};

#define allocate_vector(...) false

#define slen(__x) (sizeof(__x)/sizeof(*__x))

// just buffer -> graph please
value parse(buffer b)
{
    parser p = malloc(sizeof(struct parser)); // xxx stdlib
    p->tokens = lex(b);
    //    output(print(p->tokens));
    //    printf ("\n");

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

    value voidptr;

#if 0
    define_builtin(p, sym(__builtin_return_address), v, voidptr);
    define_builtin(p, sym(__builtin_reg_class),
                   pget(p->global, sym(types), sym(int)),
                   voidptr);
    // parameter list
    define_builtin(p, sym(__builtin_va_arg), vt, allocate_vector(voidptr, voidptr));
    define_builtin(p, sym(__builtin_va_start), vt, allocate_vector(voidptr));
#endif
    read_toplevels(p, 0, timm("types", types));

    return 0;
}
