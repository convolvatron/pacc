// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "pacc.h"

#define errorp(p, f, ...)

// why doesn't an ident have a location
static tuple make_ident(buffer b) {
    return timm(kind, sym(ident), value, b);
}

static tuple make_keyword(location f, symbol id) {
    return timm(kind, sym(keyword), id, id, location f);
}

static tuple make_number(location f, buffer s) {
    return timm(kind, sym(number), value, s, location f);
}

static tuple make_char(location f, int c) {
    return timm(kind, sym(char), value, c, location, f);
}

static character readc(buffer b)
{
    u8 r = *(u8*)b->contents + b->start;
    b->start++; // mutable!!
    return allocate_buffer(b->contents + b->start, 1 ); // utf8len
}


static boolean next(buffer b, character expect) {
    character c = readc(b);
    if (c == expect){
        b->start++;
        return true;
    }
    return false;
}

// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
static tuple read_number(buffer b) {
    buffer d = allocate_buffer();
    for (;;) {
        character c = readc(b);
        // this actually checks for hex, but oddly is safe in this case
        if (digit_of(c) > 0 || isalpha(c)) {
            b->start++;
            // general vector operation
            d = append(d, c);                    
        } else {
            return make_number(0, d);
        }
    }
    return 0;
}

static boolean nextoct(buffer b) {
    character c = readc(b);
    int x = digit_of(c);
    return toboolean((x >= 0)  && (x <= 7));
}

// Reads an octal escape sequence.
static character read_octal_char(buffer b, character c) {
    int r = digit_of(c);
    if (!nextoct(b)) return 0; //(character)r;
    r = (r << 3) | digit_of(c);
    if (!nextoct(b)) return 0; //(character)r;
    return 0; // (character)(r << 3) | digit_of(c));
}

// Reads a \x escape sequence.
static character read_hex_char(buffer b) {
    //    location p = get_pos(-2);
    location p;
    character c = readc(b);
    if (digit_of(c)< 0)
        errorf(p, "\\x is not followed by a hexadecimal character: %c", c);
    int r = 0;
    for (;; c = readc(b)) {
        int x = digit_of(c);
        if (x >= 0) {
            r = (r << 4) | x;
            continue;
        } else return 0; // (string with x as an rep)
    }
}

static character read_escaped_char(buffer b) {
    character c = readc(b);
    
    if ((c == quote) || (c == quotes) || (c == question_mark) || (c == backslash)) return c;
    if (c == sym(a)) return sym("\a");
    if (c == sym(b)) return sym("\b");
    if (c == sym(f)) return sym("\f");
    if (c == sym(n)) return sym("\n");
    if (c == sym(r)) return sym("\r");
    if (c == sym(t)) return sym("\t");
    if (c == sym(v)) return sym("\v");
    if (c == sym(x)) return read_hex_char(b);
    int x = digit_of(c);
    if ((x >= 0) && (x <= 7)) return read_octal_char(b, c);
    errorf(0, "unknown escape character: \\%c", c);
    return c;
}

static tuple read_char(buffer b) {
    character c = readc(b);
    character r = (c == backslash) ? read_escaped_char(b) : c;
    c = readc(b);
    if (c != quote)
        errorf(0, "unterminated char");
    return timm("value", aprintf("%c", c));
}

static tuple read_string(buffer b) {
    buffer d = allocate_buffer();
    for (;;) {
        if (size(b) == 0) errorf(0, "unterminated string");
        symbol c = readc(b);
        if (c == quote) break;
        if (c == backslash) {
            append(d, read_escaped_char(b));
        } else append(d, c);
    }
    return make_token(0, d);
}

static tuple read_ident(buffer b) {
    buffer d = allocate_buffer();
    for (;;) {
        character c = get(b, 0);
        // check to make sure this handles utf8
        if ((digit_of(c) > 0) || isalpha(c) || (c == underscore)) {
            d = append(d, c);
            b->start++;
            continue;
        }
        return make_ident(d);
    }
}

static tuple read_rep(buffer b, character expect, symbol t1, symbol els) {
    return make_keyword( 0, next(b, expect) ? t1 : els);
}

static tuple read_rep2(buffer b, character expect1, symbol t1, character expect2, symbol t2, symbol els) {
    if (next(b, expect1))
        return make_keyword( 0, t1);
    return make_keyword( 0, next(b, expect2) ? t2 : els);
}

// not the prettiest state machine at the ball
static tuple do_read_token(buffer b) {
    if (size(b)  == 0) 
        return timm(sym(eof), true);

    character c = get(b, 0);
    if (c ==  newline) return make_token( 0, newline);
    if (c == colon) return make_keyword( 0, colon);
    if (c == hash) return make_keyword( 0, hash);
    if (c == plus) return read_rep2(b, plus, sym(inc), equals, sym(+), sym(+=));
    if (c == asterisk) return read_rep(b, equals, sym(*=), sym(*));
    if (c == equals) return read_rep(b, equals, equals, equals);
    if (c == exclamation_point) return read_rep(b, equals, sym(!=), exclamation_point);
    if (c == ampersand) return read_rep2(b, ampersand, ampersand, equals, ampersand, sym(&=));
    if (c == vertical_bar) return read_rep2(b, vertical_bar, vertical_bar, equals, vertical_bar, sym(|=));
    if (c == caret) return read_rep(b, equals, caret, caret);
    if (c == quotes) return read_string(b);
    if (c == quote) return read_char(b);
    if (c == slash) return make_keyword( 0, next(b, equals) ? slash : sym(/=));

    if (isalpha(c) || (c == underscore)) return read_ident(b);

    int d = digit_of(c);
    if ((d >= 0)  && (d <= 9)) return read_number(b);

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
        return make_keyword(0, c);
    }
    
    if (c == minus) {
        if (next(b, minus)) return make_keyword( 0, sym(dec));
        if (next(b, greater_than)) return make_keyword( 0, sym(->));
        if (next(b, equals)) return make_keyword( 0, sym(-=));
        return make_keyword( 0, sym(-));
    }

    if (c==less_than) {
        if (next(b, less_than)) return read_rep(b, equals, sym(<<=), sym(<<));
        if (next(b, equals)) return make_keyword( 0, less_than);
        if (next(b, colon)) return make_keyword( 0, open_brace);
        if (next(b, percent)) return make_keyword( 0, open_bracket);
        return make_keyword( 0, less_than);
    }
    if (c==greater_than) {    
        if (next(b, equals)) return make_keyword( 0, sym(>=));
        if (next(b, greater_than)) return read_rep(b, equals, sym(>>=), sym(>>));
        return make_keyword( 0, greater_than);
    }
    if (c == percent) {
        return read_rep(b, equals, sym(%=), percent);
    }
    return 0;
}

buffer skip_whitespace(buffer b){
    return b;
}

tuple get_token(buffer b) {
    skip_whitespace(b);
    return do_read_token(b);
}
    
boolean is_keyword(tuple tok, symbol c) {
    return toboolean((get(tok, sym(kind)) == sym(keyword)) && (get(tok, sym(id)) == c));
}
 
