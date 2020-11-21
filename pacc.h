
// goes away
#include <runtime.h>
#include <stdio.h>

// typedef struct scope *scope;
typedef value location;
typedef value scope;
typedef scope Type;

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
