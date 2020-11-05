// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "pacc.h"
#include <string.h>
#define errorp(p, f, ...)
#include <stdlib.h>

typedef struct lexer {
    buffer b;
    u64 start;     // in bytes
    character readahead;
    
    char *resizer;
    u32 offset;
    u32 length;
} *lexer;

static inline u64 digit_of(u8 x)
{
    if ((x <= 'f') && (x >= 'a')) return x - 'a' + 10;
    if ((x <= 'F') && (x >= 'A')) return x - 'A' + 10;
    if ((x <= '9') && (x >= '0')) return x - '0';
    return -1ull;
}

static inline bits utf8_length(unsigned char x)
{
    if (~x & 0x80) return 8;
    if ((x & 0xe0) == 0xc0) return 16;
    if ((x & 0xf0) == 0xe0) return 24;
    if ((x & 0xf8) == 0xf0) return 32;
    // help
    return(1);
}

static inline boolean isdigit(u8 x)
{
    return true;
}

// ignoring all the other valid alphas...like alpha
static inline boolean isalpha(u8 x)
{
    if ((x <= 'z') && (x >= 'a')) return true;
    if ((x <= 'Z') && (x >= 'A')) return true;
    return false;
}


static inline buffer finalize(lexer lex)
{
    buffer b = allocate(tag_utf8, lex->offset);
    memcpy(contents(b), lex->resizer, lex->offset>>3);
    lex->offset = 0;
    return b;
}

// this is always the input
static inline void move(lexer lex, buffer b, bits len)
{
    if (lex->length < lex->offset + len)  {
        lex->length *= 2;
        lex->length += len; // meh
        lex->resizer = realloc(lex->resizer, lex->length);
    }
    // move what?
    lex->offset += len;
}

static void insert(lexer lex, buffer b)
{
    u8 *x = contents(b);
    // overrun 
    if ((x[0] & 0xf0) == 0xf0) return move(lex, b, 32);
    if ((x[0] & 0xe0) == 0xe0) return move(lex, b, 24);        
    if ((x[0] & 0xc0) == 0xc0) return move(lex, b, 16);                
    return move(lex, b, 8);                    
}


#define make_token(__lex, __kind, __buffer) false

// why doesn't an ident have a location
static tuple make_ident(lexer lex)
{
    return make_token(lex, identifier, lex->b);
}

static tuple make_keyword(location f, string id) {
    return make_token(lex, keyword, lex->b);
}

// doesn't have to be* a buffer, but lets see...wondering
// if its harmful to conflate the target runtime with the
// compiler runtime
static buffer readc(lexer lex)
{
    if (lex->readahead) {
        character x = lex->readahead;
        lex->readahead = 0;
        return x;
    }
    return allocate(tag_utf8, utf8_length(lex->resizer[0]));
}

static boolean next(lexer lex, character expect)
{
    return(toboolean(readc(lex)==expect));
}

// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
static tuple read_number(lexer lex, int base) {
    for (;;) {
        character c = readc(lex);
        // this actually checks for hex, but oddly is safe in this case
        if (isdigit(*(u8 *)contents(lex->b))) {
            move(lex, lex->b, 8);
        } else {
            u64 result = 0;
            
            for (int i = 0; i < lex->offset; i ++)  
                result = result * base + digit_of(*(lex->resizer + i));
            
            return timm(kind, sym(number),
                        value, s,
                        position, lex->f);
        }
    }
    return 0;
}

static tuple read_char(lexer lex) {
    character c = readc(lex);
    
    if (c == backslash) {
        character c = readc(lex);
        if ((c == quote) || (c == quotes) || (c == question_mark) || (c == backslash)) return c;
        if (c == sym(a)) return symq("\a");
        if (c == sym(b)) return symq("\b");
        if (c == sym(f)) return symq("\f");
        if (c == sym(n)) return symq("\n");
        if (c == sym(r)) return symq("\r");
        if (c == sym(t)) return symq("\t");
        if (c == sym(v)) return symq("\v");
        if (c == sym(x)) return parse_number(lex, 16);
        int x = digit_of(*(u8 *)contents(lex->b));
        if ((x >= 0) && (x <= 7)) return parse_number(lex, 8);
        errorf(0, "unknown escape character: \\%c", c);
        // no numbers really - oh, i'm wrong, character immediate
        append(d, read_escaped_char(lex));
    } else append(d, c);    
     return timm("value", aprintf("%c", c));
}

static tuple read_string(lexer lex) {
    buffer d = allocate_buffer();
    // collect offsets
    for (;;) {
        if (size(b) == 0) errorf(0, "unterminated string");
        symbol c = readc(lex);
        if (c == quote) break;
 
    }
    return make_token(0, d);
}

static tuple read_ident(lexer lex) {
    buffer d = allocate_buffer();
    for (;;) {
        character c = get(b, 0);
        if (is_digit(c) || isalpha(c) || (c == underscore)) {
            move(lex);
        } else {
            return make_ident(d);            
        }
    }
}

static tuple read_rep(lexer lex, character expect, symbol t1, symbol els) {
    return make_keyword( 0, next(lex, expect) ? t1 : els);
}

static tuple read_rep2(lexer lex,
                       character expect1, symbol t1,
                       character expect2, symbol t2,
                       symbol els) {
    if (next(b, expect1))
        return make_keyword( 0, t1);
    return make_keyword( 0, next(b, expect2) ? t2 : els);
}

// not the prettiest state machine at the ball
static token do_read_token(lexer lex)
{
    if (b->offset == b->length)
        return timm(sym(eof), true);

    character c = get(b, 0);
    if (c == newline) return make_token(lex, newline);
    if (c == colon) return make_keyword(lex, colon);
    if (c == octothorpe) return make_keyword(lex, octothorpe);
    if (c == plus) return read_rep2(b, plus, sym(inc), equals, sym(+), sym(+=));
    if (c == asterisk) return read_rep(b, equals, sym(*=), sym(*));
    if (c == equals) return read_rep(b, equals, equals, equals);
    if (c == exclamation_point) return read_rep(b, equals, sym(!=), exclamation_point);
    if (c == ampersand) return read_rep2(b, ampersand, ampersand, equals, ampersand, sym(&=));
    if (c == vertical_bar) return read_rep2(b, vertical_bar, vertical_bar, equals, vertical_bar, sym(|=));
    if (c == caret) return read_rep(b, equals, caret, caret);
    if (c == quotes) return read_string(b);
    if (c == quote) return read_char(b);
    if (c == slash) return make_keyword( lex, next(b, equals) ? slash : sym(/=));

    if (isalpha(c) || (c == underscore)) return read_ident(b);

    if (is_digit(c)) return read_number(b);

    if ((c == open_paren) || 
        (c == close_paren) || 
        (c == comma) || 
        (c == semicolon) || 
        (c == open_bracket) || 
        (c == close_bracket) || 
        (c == open_brace) || 
        (c == close_brace) || 
        (c == question_mark) || 
        (c == tilde)) {
        return make_keyword(lex, c);
    }
    
    if (c == minus) {
        if (next(b, minus)) return make_keyword(lex, sym(decrement));
        if (next(b, greater_than)) return make_keyword(lex, sym(->));
        if (next(b, equals)) return make_keyword( lex, sym(-=));
        return make_keyword( lex, sym(-));
    }

    if (c==less_than) {
        if (next(b, less_than)) return read_rep(b, equals, sym(<<=), sym(<<));
        if (next(b, equals)) return make_keyword( lex, less_than);
        if (next(b, colon)) return make_keyword( lex, open_brace);
        if (next(b, percent)) return make_keyword( lex, open_bracket);
        return make_keyword( lex, less_than);
    }
    if (c==greater_than) {    
        if (next(b, equals)) return make_keyword( lex, sym(>=));
        if (next(b, greater_than)) return read_rep(b, equals, sym(>>=), sym(>>));
        return make_keyword( lex, greater_than);
    }
    if (c == percent) {
        return read_rep(b, equals, sym(%=), percent);
    }
    return 0;
}

tuple get_token(lexer lex) {
    while (whitespace(readc(lecx)));    
    return do_read_token(lex);
}

lexer create_lex(buffer b)
{
    lexer lex = allocate(sizeof(struct lexer));
    lex->length = 64;
    lex->resizer = malloc(lex->length);
    lex->offset = 0;
    lex->b = b;
    
    return lex;
}
