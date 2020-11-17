// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "pacc.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// a dodge for the moment, we only ever are interested in the first byte
typedef u8 character; 

#define unread(__lex) ((__lex)->scan -= 8)

struct lexer {
    buffer b;
    u64 scan;
    u64 start;       // for extents
    value tokens; // per-lex, right? sure, but a set please 
    
    u8 *resizer;
    bits offset;
    bits length;
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

// bitmap character set?
static inline boolean iswhitespace(character x)
{
    return toboolean((x == ' ') || (x == '\t')|| (x == '\n'));
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

// this is always the input - is it? is it really?
// ok, break it out if it isn't?
static inline void move(lexer lex, unsigned char *source, bits length)
{
    if (lex->length < lex->offset + length)  {
        lex->length *= 2;
        lex->length += length; // meh
        lex->resizer = realloc(lex->resizer, lex->length);
    }
    memcpy(lex->resizer, source, length);
}

// assuming start fits in a small
#define make_token_thing(__lex, __kind, __v)    \
    ({                                          \
    value r = timm(sym(kind), sym(#__kind),     \
                   sym(start), lex->start,      \
                   sym(end), lex->scan,         \
                   sym(value), __v);            \
    buffer b = print(r);                        \
    lex->start = lex->scan;                     \
    r;                                          \
    })

    
#define lexbuffer(__lex) allocate_utf8(__lex->resizer + bytesof(__lex->start), \
                                       bytesof(lex->offset))

#define sourcebuffer(__lex) allocate_utf8(contentsu8(lex->b)+bytesof(lex->start), \
                                          bytesof((lex->scan - lex->start)))

#define make_token(__lex, __kind) make_token_thing(__lex, __kind, lexbuffer(__lex))


static inline character readc(lexer lex)
{
    u8 res = contentsu8(lex->b)[bytesof(lex->scan)];
    lex->scan += 8;    
    return res;
}

// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
static tuple read_number(lexer lex, int base) {
    for (;;) {
        u64 result = 0;        
        character c = readc(lex);
        if (isdigit(c, base)) {
            result = result * base + digit_of(c);
        } else {
            unread(lex);
            return make_token_thing(lex, number, result);
        }
    }
    return 0;
}

// consider removing this from the target language
// also utf8 character constants?
static tuple read_character_constant(lexer lex) {
    character c = readc(lex);
    
    if (c == '\\') {
        character c = readc(lex);
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
            errorf(0, "unknown escape character: \\%c", c);
        }
    }
    if (readc(lex) != '"') errorf(0, "unterminated character constant");
    return make_token_thing(lex, value, c); 
}

static tuple read_string(lexer lex)
{
    for (;;) {
        if (lex->offset == (u64)length(lex->b)) errorf(0, "unterminated string");
        character c = readc(lex);
        if (c == '"') break;
    }
    return make_token(lex, string);
}

static tuple read_ident(lexer lex) {
    for (;;) {
        character c = readc(lex);
        if (isdigit(c, 10) || isalpha(c) || (c == '_')) {
            move(lex, (u8 *)contents(lex->b), utf8_length(*(u8 *)contents(lex->b)));
        } else {
            // consider just keeping a reference to the source rather than
            // building up a string - just a thought
            unread(lex);
            return make_token(lex, identifier);
        }
    }
}

// not the prettiest state machine at the ball
// static maps would get us a proper one i guess
tuple get_token(lexer lex) {
    character c = readc(lex);
    while (iswhitespace(c)) c = readc(lex);
    if (lex->start == lex->b->length) 
        return timm(sym(kind), sym(eof));

    // we are eating this - i guess cpp wants it
    //    if (c == newline) return make_token(lex, newline);
    if (c == '"') return read_string(lex);
    if (c == '\'') return read_character_constant(lex);
    if (isalpha(c) || (c == '_')) return read_ident(lex);
    if (isdigit(c, 10)) return read_number(lex, 10);

    while (table_get(lex->tokens, sourcebuffer(lex))) lex->scan+=8;
    lex->scan-=8;
    if (lex->scan > lex->start) 
        return make_token_thing(lex, keyword, sourcebuffer(lex));
    
    halt("token fail");
}

void build(lexer lex, ...)
{
    char *x;
    int total = 0; 
    foreach_arg(lex, i) total++;
    lex->tokens = allocate_table(total);
    foreach_arg(lex, i) 
        table_insert(lex->tokens, stringify(i), true);
}
    
lexer create_lex(buffer b)
{
    lexer lex = malloc(sizeof(struct lexer)); // malloc?
    lex->length = 64;
    lex->resizer = malloc(lex->length);
    lex->offset = 0;
    lex->b = b;

    build(lex, "*", "*=", "&", "&=", "&&", "==", "=", "^=", "^", "#", ":", "|", "|=",
          "(", ")", "[", "]", "{", "}", ";", ",", "?", "~", "--", "->", "-=", "<<", "/", "/=",
          "<=", "<:", "<%", ">=", ">>", ">", "%", "%=", INVALID_ADDRESS);
    return lex;
}
