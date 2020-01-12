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
    b->start++;
    return r;
}


static boolean next(buffer b, int expect) {
    u8 c = readc(b);
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
        int c = readc(b);
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
    int c = readc(b);
    return toboolean('0' <= c && c <= '7');
}

// Reads an octal escape sequence.
static int read_octal_char(buffer b, int c) {
    int r = c - '0';
    if (!nextoct(b))
        return r;
    r = (r << 3) | (readc(b) - '0');
    if (!nextoct(b))
        return r;
    return (r << 3) | (readc(b) - '0');
}

// Reads a \x escape sequence.
static int read_hex_char(buffer b) {
    //    location p = get_pos(-2);
    int c = readc(b);
    if (digit_of(c)< 0)
        errorp(p, "\\x is not followed by a hexadecimal character: %c", c);
    int r = 0;
    for (;; c = readc(b)) {
        switch (c) {
        case '0' ... '9': r = (r << 4) | (c - '0'); b->start++;continue;
        case 'a' ... 'f': r = (r << 4) | (c - 'a' + 10);b->start++; continue;
        case 'A' ... 'F': r = (r << 4) | (c - 'A' + 10);b->start++; continue;
        default: return r;
        }
    }
}

static int read_escaped_char(buffer b) {
    //    location p = get_pos(-1);
    int c = readc(b);

    switch (c) {
    case '\'': case '"': case '?': case '\\':
        return c;
    case 'a': return '\a';
    case 'b': return '\b';
    case 'f': return '\f';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case 'v': return '\v';
    case 'e': return '\033';  // '\e' is GNU extension
    case 'x': return read_hex_char(b);
    case '0' ... '7': return read_octal_char(b, c);
    }
    errorp(p, "unknown escape character: \\%c", c);
    return c;
}

static tuple read_char(buffer b) {
    int c = readc(b);
    int r = (c == '\\') ? read_escaped_char(b) : c;
    c = readc(b);
    if (c != '\'')
        errorp(pos, "unterminated char");
    return timm("value", aprintf("%c", c));
}

// Reads a string literal.
static tuple read_string(buffer b) {
    buffer d = allocate_buffer();
    for (;;) {
        if (size(b) == 0)  errorp(pos, "unterminated string");
        int c = readc(b);
        if (c == '"')
            break;
        if (c != '\\') {
            append(d, c);
            continue;
        }
        c = read_escaped_char(b);
        append(d, c);        
    }
    return make_token(0, d);
}

static tuple read_ident(buffer b) {
    buffer d = allocate_buffer();
    for (;;) {
        character c = get(b, 0);
        // check to make sure this handles utf8
        if ((digit_of(c) > 0) || isalpha(c) || (c & 0x80) || c == '_' || c == '$') {
            append(d, c);
            b->start++;
            continue;
        }
        return make_ident(d);
    }
}

static tuple read_rep(buffer b, char expect, symbol t1, symbol els) {
    return make_keyword( 0, next(b, expect) ? t1 : els);
}

static tuple read_rep2(buffer b, char expect1, symbol t1, char expect2, symbol t2, symbol els) {
    if (next(b, expect1))
        return make_keyword( 0, t1);
    return make_keyword( 0, next(b, expect2) ? t2 : els);
}

// not the prettiest state machine at the ball
static tuple do_read_token(buffer b) {
    if (size(b)  == 0) 
        return timm(sym(eof), true);

    character c = get(b, 0);
    switch (c) {
    case '\n': return make_token( 0, newline);
    case ':': return make_keyword( 0, sym(:));
    case '#': return make_keyword( 0, sym(#));
    case '+': return read_rep2(b, '+', sym(inc), '=', sym(+), sym(+=));
    case '*': return read_rep(b, '=', sym(*=), sym(*));
    case '=': return read_rep(b, '=', sym(=), sym(=));
    case '!': return read_rep(b, '=', sym(!=), sym(!));
    case '&': return read_rep2(b, '&', sym(&), '=', sym(&), sym(&=));
    case '|': return read_rep2(b, '|', sym(|), '=', sym(|), sym(|=));
    case '^': return read_rep(b, '=', sym(^), sym(^));
    case '"': return read_string(b);
    case '\'': return read_char(b);
    case '/': return make_keyword( 0, next(b, '=') ? sym(/) : sym(/));
    case 'a' ... 'z': case 'A' ... 'Z': case '_': case '$':
    case 0x80 ... 0xFD:
        // c is unconsumed
        return read_ident( b);
    case '0' ... '9':
        return read_number(b);
    case '.':
        if (digit_of(readc(b)) > 0)
            return read_number(b);
        if (next(b, '.')) {
            if (next(b, '.'))
                return make_keyword( 0, sym(...));
            return make_ident( sym(..));
        }
        return make_keyword( 0, sym(.));
    case '(': case ')': case ',': case ';': case '[': case ']': case '{':
    case '}': case '?': case '~':{
        char k[2]={c, 0};
        return make_keyword( 0, c);
    }
    case '-':
        if (next(b, '-')) return make_keyword( 0, sym(dec));
        if (next(b, '>')) return make_keyword( 0, sym(->));
        if (next(b, '=')) return make_keyword( 0, sym(-=));
        return make_keyword( 0, sym(-));
    case '<':
        if (next(b, '<')) return read_rep(b, '=', sym(<<=), sym(<<));
        if (next(b, '=')) return make_keyword( 0, sym(<));
        if (next(b, ':')) return make_keyword( 0, symq("["));
        if (next(b, '%')) return make_keyword( 0, symq("{"));
        return make_keyword( 0, sym(<));
    case '>':
        if (next(b, '=')) return make_keyword( 0, sym(>=));
        if (next(b, '>')) return read_rep(b, '=', sym(>>=), sym(>>));
        return make_keyword( 0, sym(>));
    case '%': {
        return read_rep(b, '=', sym(%=), sym(%));
    }
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
 
