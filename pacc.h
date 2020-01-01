#include <runtime.h>

typedef struct scope *scope;
typedef struct table *tuple;
typedef tuple location;

typedef tuple Type;

typedef struct parse {
    buffer b;

    // umm
    scope env;
    scope file;
    scope global;    
} *parse;


typedef tuple Node;

#if 0
typedef struct Node {
    symbol kind;
    Type *ty; // -> type
    location sourceLoc;
    //    parse p;
    union {
        // Char, int, or long
        long ival;
        // String
        struct {
            buffer sval;
            char *slabel;
        };
        // Local/global variable
        struct {
            buffer varname;
            // local
            int loff;
            vector lvarinit;
            // global
            buffer glabel;
        };
        // Binary operator
        struct {
            struct Node *left;
            struct Node *right;
        };
        // Unary operator
        struct {
            struct Node *operand;
        };
        // Function call or function declaration
        struct {
            string fname;
            // Function call
            vector args;
            struct Type *ftype;
            // Function pointer or function designator
            struct Node *fptr;
            // Function declaration
            vector params;
            vector localvars;
            struct Node *body;
        };
        // Declaration
        struct {
            struct Node *declvar;
            vector declinit;
        };
        // Initializer
        struct {
            struct Node *initval;
        };
        // If statement or ternary operator
        struct {
            struct Node *cond;
            struct Node *then;
            struct Node *els;
        };
        // Goto and label
        struct {
            buffer label;
            buffer newlabel;
        };
        // Return statement
        struct Node *retval;
        // Compound statement
        vector stmts;
        // Struct reference
        struct {
            struct Node *struc;
            buffer field;
            Type *fieldtype;
        };
    };
} Node;
#endif

typedef struct {
    int beg;
    int end;
    buffer label;
} Case;

tuple parse_init(buffer b);

#define error(...)       
#define errort(tok, ...) 
#define warn(...)        
#define warnt(tok, ...)  

void errorf(char *line, char *pos, char *fmt, ...);
void warnf(char *line, char *pos, char *fmt, ...);

boolean is_keyword(tuple tok, symbol c);
tuple get_token(buffer b);

tuple parse_init(buffer);

scope allocate_scope(heap h, scope parent);
#define INVALID_ADDRESS (-1ull)
#define timm(...) ((tuple)0)
