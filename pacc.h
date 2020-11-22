
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
typedef struct parser *parser;
Node conv(parser p, Node node);
Node read_subscript_expr(parser p, scope env, Node node);
static Node read_expr(parser p, scope env);
Node conv(parser p, Node node);


#define pget(__e, ...) pget_internal(__e, __VA_ARGS__, INVALID_ADDRESS)
static value pget_internal(void *e, ...)
{
    foreach_arg(e, i) e = get(e, i);
    return e;
}

value token(parser);
boolean next_token(parser p, string kind);
string make_label();
void read_decl(parser p, scope env, vector block);
