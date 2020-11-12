#include <runtime.h>
#include <stdio.h>

typedef value tuple;

typedef struct scope *scope;
typedef tuple location;

typedef scope Type;
typedef scope token;

typedef struct parser {
    token readahead;
    
    scope env;
    scope file;
    scope global;    
} *parser;


typedef tuple Node;

tuple parse_init(buffer b);

#define error(...)       
#define errort(tok, ...) 

void errorf(tuple f, char *fmt, ...);

boolean is_keyword(tuple tok, string c);
typedef struct lexer *lexer;
tuple get_token(lexer lex);

tuple parse_init(buffer);

tuple allocate_tuple(tuple parent);
#define INVALID_ADDRESS ((void *)(-1ull))

static inline value timm_internal(value trash, ...)
{
    int total = 0;                              
    foreach_arg(trash, i) total++;
    if (total &1) {halt("key without value in timm"); } 
    buffer t = allocate_table(total/2);
    value key;
    foreach_arg(trash, i) {
        if (total++&1) {
            table_insert(t, key, i);
        } else {
            key = i;
        }
    }
    return t;
}

#define timm(...) timm_internal(0, __VA_ARGS__, INVALID_ADDRESS);


tuple make_token(value, string);
lexer create_lex(buffer b);

#define errorp(p, f, ...)

typedef struct lexer *lexer;
