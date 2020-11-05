#include <runtime.h>

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
tuple get_token(buffer b);

tuple parse_init(buffer);

tuple allocate_tuple(tuple parent);
#define INVALID_ADDRESS ((void *)(-1ull))

#define timm(...) ((tuple)0)
#define simm(...) ((scope)0)

string close_paren, open_paren, close_brace, open_brace,
    close_bracket, open_bracket, less_than, greater_than, semicolon,
    colon, underscore, quotes, space, comma, percent, quote,
    backslash, question_mark, newline, asterisk, ampersand, slash,
    caret, hash, exclamation_point, plus, minus, equals, vertical_bar,
    tilde, period;

tuple make_token(value, string);

