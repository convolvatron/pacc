
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

boolean is_keyword(tuple tok, string c);

typedef struct lexer *lexer;
tuple get_token(lexer lex);

value parse(lexer lex);

tuple allocate_tuple(tuple parent);


tuple make_token(value, string);
lexer create_lex(buffer b);

#define errorp(p, f, ...)

typedef struct lexer *lexer;
