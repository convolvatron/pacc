// according to the immutable worlds model we should probably do utf8 as a seperate
// pass and let the compiler worry about interleaving/scheduling the evalution
#include "pacc.h"
#include <stdio.h>

#define lerror(...)

// should get flattened out
struct lexer {
    buffer b;
    value keywords;
    // move to world ?  also backslash translations
    value whitespace; 
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

static int character_rewrites[] = 
    {    'a', '\a',
         'b','\b',
         'f','\f',
         'n','\n',
         'r','\r',
         't','\t',
         'v','\v'
    };
    
    
// consider removing this from the target language
// also utf8 character constants?
static u64 read_character_constant(lexer lex, u64 offset) {
    u64 end;
    character c = readc(lex->b, offset, end);

    static value cws;
    if (!cws) {
        cws = allocate_table(slen(character_rewrites)/2);
        for (int i = 0; i < slen(character_rewrites); i+=2)
            table_insert(cws,
                         (value)(u64)character_rewrites[i],
                         (value)(u64)character_rewrites[i+1]);
    }
    
    if (c == '\\') {
        character c = readc(lex->b, end, end);
        character cp = (u64)get(cws, (value)c);
        //        if (!(((c == '\'') || (c == '"') || (c == '?') || (c == '\\')))) {
        if (cp) {
            // xxx - immediate unicode(?) syntax?
            //   if (c == sym(x)) return parse_number(lex, 16);
            //   int x = digit_of(*(u8 *)contents(lex->b));
            //   if ((x >= 0) && (x <= 7)) return parse_number(lex, 8);
        } else lerror(p, "unknown escape character: \\%c", cp);

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

u64 scan_keyword(lexer lex, u64 start, value keywords)
{
    // xxx - disabling unary for a second -- can just union it here in c
    u64 scan = start, next;
    while ((next = scan + utf8_length(characterof(lex->b, scan))),
           (scan < lex->b->length) && 
           pget(keywords, substring(lex->b, start, next))) {
        scan = next;
    }
    
    if (scan > start) {
        make_token(lex->out, start, scan, keyword, substring(lex->b, start, scan));
        return scan;
    }
    return start;
}

static u64 choose(lexer lex, u64 scan, value keywords)
{
    u64 _;
    
    if (lex->b->length == scan) return scan;
    
    character c = readc(lex->b, scan, _);

    // there are more here
    if (c == ';') {
        make_token(lex->out, scan, scan+1, keyword, stringify(";"));
        return scan+8;
    }
    if (c == '"') return read_string(lex, scan);
    if (c == '\'') return read_character_constant(lex, scan);
    if (isalpha(c) || (c == '_')) return read_ident(lex, scan);
    if (isdigit(c, 10)) return read_number(lex, scan, 10);
    
    u64 next = scan_keyword(lex, scan, keywords);
    if (next == scan) halt("lex error", c);
    return next;
}

// schema - backslashes
//          whitespaces
//          keywords

vector lex(buffer b, value keywords)
{
    lexer lex = malloc(sizeof(struct lexer)); // malloc?
    lex->out = allocate_nursery(128);
    lex->b = b;
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
        scan = choose(lex, scan, keywords);
    }
    value result = allocate_table(lex->out->offset/bitsizeof(value));

    // should have a vector representation - once we land a little
    for (u64 i =0; i<lex->out->offset/bitsizeof(value); i++) {
        value v = ((value*)(lex->out->resizer))[i];
        table_insert(result, (value)i, v);
    }
    return result;
}
