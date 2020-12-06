// according to the immutable worlds model we should probably do utf8 as a seperate
// pass and let the compiler worry about interleaving/scheduling the evalution
#include "pacc.h"
#include <stdlib.h>
#include <stdio.h>

typedef struct elastic { //ahem
    u8 *resizer;
    bits offset;
    bits length;
} *elastic;

// should go 
struct lexer {
    buffer b;

    value tokens; // per-lex, right? sure, but a set please
    value whitespace; // per-lex, right? sure, but a set please    
    value backslash_translations;
    
    elastic r;
    elastic out;
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


static inline void push_mut(elastic e, unsigned char *source, bits length)
{
    if (e->length < e->offset + length)  {
        e->length *= 2;
        e->length += length; // meh
        e->resizer = realloc(e->resizer, e->length);
    }
    __builtin_memcpy(e->resizer + e->offset, source, bytesof(length));
    e->offset += length;
}

elastic allocate_elastic()
{
    elastic e = malloc(sizeof(struct elastic));
    e->offset = 0;
    e->length = 128;    
    e->resizer = malloc(e->length);
    return e;
}


// assuming start fits in a small
// do we really want to update offset here? i know we're
// not supposed to care about layering at this scope, but..
#define make_token(__lex, __start, __end, __kind, __v)   \
    ({                                       \
    value r = timm(sym(kind), sym(__kind),   \
                   sym(start), __start,   \
                   sym(end), __end,      \
                   sym(value), __v);         \
    push_mut(__lex, (void *)&r, bitsizeof(value));      \
    })

#define readc(__b, __offset, __next) ({                         \
            character x = characterof(__b, __offset);           \
            __next = __offset + utf8_length(x);                 \
            x;                                                  \
            })
            
// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
// assume short
static u64 read_number(lexer lex, int offset, int base) {
    u64 result = 0, end = offset;            
    for (;;) {
        character c = (u32)readc(lex->b, offset, end);
        if (isdigit(c, base)) {
            result = result * base + digit_of(c);
            offset = end;
        } else {
            make_token(lex->out, offset, end, number, (value)result);
        }
    }
    // its in the token?
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
            error(p, "unknown escape character: \\%c", c);
        }
    }
    if (readc(lex->b, end, end) != '"') error(p, "unterminated character constant", lex);
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
            error(p, "unterminated string");
        character c = readc(lex->b, end, end);
        if (c == '"') break;
    }
    make_token(lex->out, offset, end, string, substring(contents(lex->b), offset, end));
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
    
vector lex(buffer b)
{
    lexer lex = malloc(sizeof(struct lexer)); // malloc?
    lex->out = allocate_elastic();
    lex->b = b;
    // sleezy workaround to avoid a linear chain...however, linear isn't
    // so bad?   (value:int next:(value:main next:(value:eof)))

    value tokens = set_of_strings("*", "*=", "&", "&=", "&&", "==", "=", "^=", "^", "#",
                                  ":", "|", "|=", "(", ")", "[", "]", "{", "}", ";", ",", "?",
                                  "~", "--", "->", "-=", "<<", "/", "/=", "<=", "<:", "<%",
                                  ">=", ">>", ">", "%", "%=", INVALID_ADDRESS);
    
    build_backslashes(lex, a, b, f, n, r, t, v, INVALID_ADDRESS);
    value whitespace = set ((value)' ',
                            (value)'\t',
                            (value)'\n', INVALID_ADDRESS);
    u64 scan = 0, _; 

    output(print(whitespace));
    while (scan < b->length) {
        character c = readc(lex->b, scan, _);
        printf ("lex: %c\n", (char)c);
        // double get
        while (table_get(whitespace, (value)c)) 
            c = readc(lex->b, scan, scan);

        // nescan one of these
        if (c == '"') scan = read_string(lex, scan);
        if (c == '\'') scan = read_character_constant(lex, scan);
        if (isalpha(c) || (c == '_')) scan = read_ident(lex, scan);
        if (isdigit(c, 10)) scan = read_number(lex, 10, scan);

        u64 start = scan;
        printf("lex: %lld %lld\n", scan, start);
        while ((c = characterof(lex->b, start)), table_get(tokens, (value)c))
            scan+=utf8_length(c);
        
        if (scan > start) {
            make_token(lex->out, start, scan, keyword, substring(lex->b, start, scan));
        } 
    }
    printf ("lex results: %llx\n", lex->out->offset);
    value result = allocate_table(lex->out->offset/bitsizeof(value));

    // should have a vector representation - once we land a little
    for (u64 i =0; i<lex->out->offset/bitsizeof(value); i++)
        table_insert(result, (value)i, ((value *)(lex->out->resizer))[i]);

    output(print(result));
    printf("\n");
    return result;
}
