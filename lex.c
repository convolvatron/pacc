// according to the immutable worlds model we should probably do utf8 as a seperate
// pass and let the compiler worry about interleaving/scheduling the evalution
#include "pacc.h"
#include <stdlib.h>
#include <stdio.h>

typedef u32 character; 


static inline buffer substring(void *base, bits start, bits end)
{
    return allocate_utf8(base+(start>>3), end-start);
}


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

static inline bits utf8_length(character x)
{
    if (~x & 0x80) return 8;
    if ((x & 0xe0) == 0xc0) return 16;
    if ((x & 0xf0) == 0xe0) return 24;
    if ((x & 0xf8) == 0xf0) return 32;
    halt("invalid utf8 character");
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
    e->offset += bytesof(length);
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

    
#define elbuffer(__e) allocate_utf8(__e->resizer, __e->offset)

#define sourcebuffer(__lex) allocate_utf8(contentsu8(lex->b)+bytesof(lex->start), \
                                          bytesof((lex->scan - lex->start)))

// broken for multibyte - intern?
#define readc(__b, __offset) ({                                 \
    character x = contentsu8(__b)[bytesof(*(__offset))];        \
    *(__offset) = *(__offset) + utf8_length(x);                 \
    x;                                                          \
        })
        
// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
// assume short
static u64 read_number(lexer lex, int offset, int base) {
    u64 result = 0, end = offset;            
    for (;;) {
        character c = readc(lex->b, &offset);
        if (isdigit(c, base)) {
            result = result * base + digit_of(c);
        } else {
            make_token(lex->out, offset, end, number, (value)result);
        }
    }
    return offset;
}

// consider removing this from the target language
// also utf8 character constants?
static u64 read_character_constant(lexer lex, u64 offset) {
    character c = readc(lex->b, &offset);
    u64 end = offset;
    
    if (c == '\\') {
        character c = readc(lex->b, &end);
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
    if (readc(lex->b, &end) != '"') error(p, "unterminated character constant", lex);
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
        character c = readc(lex->b, &end);
        if (c == '"') break;
    }
    make_token(lex->out, offset, end, string, substring(contents(lex->b), offset, end));
    return offset;
}

// i used to have this running through the resizer buffer
static u64 read_ident(lexer lex, u64 offset) {
    u64 end = offset;
    for (;;) {
        character c = readc(lex->b, &end);
        if (!(isdigit(c, 10) || isalpha(c) || (c == '_'))){
            make_token(lex->out, offset, end, identifier,
                       substring(lex->b, offset, end));
            return end;
        }
    }
}

#define build_backslashes(__lex, ...) build_backslashes_internal(__lex)
void build_backslashes_internal(lexer lex, ...)
{
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
    value whitespace = set_of_strings (" ", "\t", "\n");
    u64 scan = 0; 

    while (scan < b->length) {
        character c = readc(lex->b, &scan);
        while (table_get(whitespace, (value)c)) c = readc(lex->b, &scan);
        
        if (c == '"') scan = read_string(lex, scan);
        if (c == '\'') scan = read_character_constant(lex, scan);
        if (isalpha(c) || (c == '_')) scan = read_ident(lex, scan);
        if (isdigit(c, 10)) scan = read_number(lex, 10, scan);

        // we can use readc here
        u64 start = scan;
        while (table_get(tokens, substring(lex->b, start, scan))) scan+=8;
        scan-=8;
        
        if (scan > start) 
            make_token(lex->out, start, scan, keyword, substring(lex->b, start, scan));
    }
        
    value result = allocate_table(lex->out->offset/bitsizeof(value));

    // should have a vector representation - once we land a little
    for (u64 i =0; i<lex->out->offset/bitsizeof(value); i++)
        table_insert(result, (value)i, ((value *)(lex->out->resizer))[i]);
    
    return result;
}
