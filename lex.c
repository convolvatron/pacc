// according to the immutable worlds model we should probably do utf8 as a seperate
// pass and let the compiler worry about interleaving/scheduling the evalution
#include "pacc.h"
#include <stdio.h>

#define lerror(...)

// should go 
struct lexer {
    buffer b;

    value tokens; // per-lex, right? sure, but a set please
    value whitespace; // per-lex, right? sure, but a set please    
    value backslash_translations;
    
    //    nursery r;
    nursery out;
};

static inline u64 digit_of(character x)
{
    if ((x <= 'f') && (x >= 'a')) return x - 'a' + 10;
    if ((x <= 'F') && (x >= 'A')) return x - 'A' + 10;
    if ((x <= '9') && (x >= '0')) return x - '0';
    return -1ull;
}

static inline boolean isdigit(character x, int base)
{
    // broken for hex
    return toboolean((x <= '9') && (x >= '0'));
}

// ignoring all the other valid alphas...like alpha
static inline boolean isalpha(character x)
{
    if ((x <= 'z') && (x >= 'a')) return true;
    if ((x <= 'Z') && (x >= 'A')) return true;
    return false;
}

// assuming start fits in a small
// do we really want to update offset here? i know we're
// not supposed to care about layering at this scope, but..
#define make_token(__lex, __start, __end, __kind, __v)   \
    ({                                       \
    value r = timm(sym(kind), sym(__kind),   \
                   sym(start), __start,   \
                   sym(end), __end,      \
                   sym(value), __v);     \
    push_mut(__lex, (void *)&r, bitsizeof(value));      \
    })

// eof processing in readc
#define readc(__b, __offset, __next) ({                         \
            if (__offset >= __b->length) halt("overread");        \
            character x = characterof(__b, __offset);           \
            __next = __offset + utf8_length(x);                 \
            x;                                                  \
            })
            
// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
// assume short
static u64 read_number(lexer lex, int offset, int base) {
    u64 result = 0, end = offset, start= offset;
    character c;
    while (c = (u32)readc(lex->b, offset, end), isdigit(c, base)) {
        result = result * base + digit_of(c);
        offset = end;
    }
    if (offset > start) 
        make_token(lex->out, start, offset, number, (value)result);

    return offset;
}

// consider removing this from the target language
// also utf8 character constants?
static u64 read_character_constant(lexer lex, u64 offset) {
    u64 end;
    character c = readc(lex->b, offset, end);
    
    if (c == '\\') {
        character c = readc(lex->b, end, end);
        if (!(((c == '\'') || (c == '"') || (c == '?') || (c == '\\')))) {
            if (c == 'a') c = '\a';
            if (c == 'b') c = '\b';
            if (c == 'f') c = '\f';
            if (c == 'n') c = '\n';
            if (c == 'r') c = '\r';
            if (c == 't') c = '\t';
            if (c == 'v') c = '\v';
            // xxx - immediate unicode(?) syntax?
            //   if (c == sym(x)) return parse_number(lex, 16);
            //   int x = digit_of(*(u8 *)contents(lex->b));
            //   if ((x >= 0) && (x <= 7)) return parse_number(lex, 8);
            lerror(p, "unknown escape character: \\%c", c);
        }
    }
    if (readc(lex->b, end, end) != '"') lerror(p, "unterminated character constant", lex);
    make_token(lex->out, offset, end, value, (value)(u64)c);
    return end;
}

// this needs the reassembly buffer and the \ translation
static u64 read_string(lexer lex, u64 offset)
{
    u64 end = offset;
    for (;;) {
        // we can keep the open token on hand for just such an event
        if (offset == (u64)length(lex->b))
            lerror(p, "unterminated string");
        character c = readc(lex->b, end, end);
        if (c == '"') break;
    }
    make_token(lex->out, offset, end, string, substring(lex->b, offset, end));
    return offset;
}

// i used to have this running through the resizer buffer
static u64 read_ident(lexer lex, u64 offset) {
    u64 end = offset;
    for (;;) {
        u64 next;
        character c = readc(lex->b, end, next);
        if (!(isdigit(c, 10) || isalpha(c) || (c == '_'))){
            make_token(lex->out, offset, end, identifier,
                       substring(lex->b, offset, end));
            return end;
        }
        end = next;
    }
}

#define build_backslashes(__lex, ...) build_backslashes_internal(__lex)
void build_backslashes_internal(lexer lex, ...)
{
}


value set(value x, ...)
{
    int total = 1;
    foreach_arg(x, i) total++;
    value t = allocate_table(total);
    //sets
    table_insert(t, x, true);    
    foreach_arg(x, i) table_insert(t, i, true);
    return t;
}

value set_of_strings(char *x, ...)
{
    int total = 1;
    foreach_arg(x, i) total++;
    value t = allocate_table(total);
    //sets
    table_insert(t, stringify(x), true);    
    foreach_arg(x, i) table_insert(t, stringify(i), true);
    return t;
}


#define right 0
#define left 1
#define prefix 2
#define postfix 3 

#define unary 1
#define binary 1
#define ternary 3

struct operator {char *name; int precedence; int arity; int associativity;};
    
struct operator ops[] =
    {
     {"." , 1,  binary,  left},
     {"(" , 1,  unary,   left},
     {"[" , 1,  unary,   left},       
     {"&" , 1,  unary,   right},
     {"->", 1,  binary,  left},     
     {"--", 1,  unary,   postfix},
     {"++", 1,  unary,   postfix},
     {"~" , 2,  unary,   right},
     {"!" , 2,  unary,   right},     
     {"--", 2,  unary,   prefix},
     {"++", 2,  unary,   prefix},
     {"*" , 2,  unary,   right},     
     {"/" , 3,  binary,  left},
     {"%" , 3,  binary,  left},     
     {"*" , 3,  binary,  left},
     {"+" , 4,  binary,  left},
     {"-" , 4,  binary,  left}, 
     {"<<", 5,  binary,  right},
     {">>", 5,  binary,  right},
     {"<=", 6,  binary,  left},
     {">=", 6,  binary,  left},
     {">" , 6,  binary,  left},
     {">" , 6,  binary,  left},          
     {"==", 7,  binary,  left},
     {"!=", 7,  binary,  left},
     {"&" , 8,  binary,  left},     
     {"^" , 9,  binary,  left},
     {"|" , 10, binary,  left},
     {"&&", 11, binary,  left}, 
     {"||", 12, binary,  left},
     {":" , 13, ternary, right},     
     {"?" , 13, ternary, right},
     {"|=", 14, binary,  right},
     {"%=", 14, binary,  right},
     {"-=", 14, binary,  right},
     {"/=", 14, binary,  right},
     {"*=", 14, binary,  right},
     {"&=", 14, binary,  right},
     {"=" , 14, binary,  right},
     {"^=", 14, binary,  right},
     {"," , 15, binary,  left},
};

// array index, function - what are you doing here '{' .. oh, just lexical
// char *others[] = {"#","(", ")", "[", "]", "{", "}"};

u64 scan_operator(lexer lex, u64 start)
{
    // move dataspace construction up - ideally we have a static facts store
    static value operators; 
    if (!operators) {
        operators = allocate_table(slen(ops));
        for (struct operator *o = ops; o < (ops + slen(ops)); o++){
            table_insert(operators, stringify(o->name),
                         timm(sym(arity),         o->arity,
                              sym(associativity), o->associativity,
                              sym(precedence),    o->precedence));
        }
    }

    u64 scan = start, next;
    while ((next = scan + utf8_length(characterof(lex->b, scan))),
           (scan < lex->b->length) && 
           table_get(operators, substring(lex->b, start, next))) {
        scan = next;
    }
    
    if (scan > start) {
        make_token(lex->out, start, scan, keyword, substring(lex->b, start, scan));
        return scan;
    }
    return start;
}

static u64 choose(lexer lex, u64 scan)
{
    u64 _;
    
    if (lex->b->length == scan) return scan;
    
    character c = readc(lex->b, scan, _);
    
    if (c == '"') return read_string(lex, scan);
    if (c == '\'') return read_character_constant(lex, scan);
    if (isalpha(c) || (c == '_')) return read_ident(lex, scan);
    if (isdigit(c, 10)) return read_number(lex, scan, 10);
    
    u64 next = scan_operator(lex, scan);
    if (next == scan) halt("lex error", c);
    return next;
}

vector lex(buffer b)
{
    lexer lex = malloc(sizeof(struct lexer)); // malloc?
    lex->out = allocate_nursery(128);
    lex->b = b;
    // sleezy workaround to avoid a linear chain...however, linear isn't
    // so bad?   (value:int next:(value:main next:(value:eof)))
    
    build_backslashes(lex, a, b, f, n, r, t, v, INVALID_ADDRESS);
    value whitespace = set ((value)' ',
                            (value)'\t',
                            (value)'\n', INVALID_ADDRESS);
    u64 scan = 0, next; 
    
    while (scan < b->length) {
        character c = readc(lex->b, scan, next);
        while (table_get(whitespace, (value)c)) {
            scan = next;
            c = readc(lex->b, scan, next);
        }
        scan = choose(lex, scan);
    }
    value result = allocate_table(lex->out->offset/bitsizeof(value));

    // should have a vector representation - once we land a little
    for (u64 i =0; i<lex->out->offset/bitsizeof(value); i++) {
        value v = ((value*)(lex->out->resizer))[i];
        table_insert(result, (value)i, v);
    }
    return result;
}
