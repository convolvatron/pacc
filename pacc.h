
// goes away
#include <runtime.h>
#include <stdio.h>

// typedef struct scope *scope;
typedef value location;
typedef value scope;
typedef scope Type;

// death to streaming
typedef struct lexer *lexer;
typedef struct parser *parser;
typedef value tuple;
typedef tuple Node;

value parse_init(lexer lex);

#define error(...)       
#define errort(tok, ...) 

void errorf(void *x, char *fmt, ...);

boolean is_keyword(tuple tok, string c);
typedef struct lexer *lexer;
tuple get_token(lexer lex);

value parse(lexer lex);

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

#define timm(...) timm_internal(0, __VA_ARGS__, INVALID_ADDRESS)


tuple make_token(value, string);
lexer create_lex(buffer b);

#define errorp(p, f, ...)

typedef struct lexer *lexer;
