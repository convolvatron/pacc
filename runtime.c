#include <stdio.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>
#include <runtime.h>
#include <unistd.h>
#include <nursery.h>

u64 fills[tag_max];
u64 lengths[tag_max];
u64 world_length = 32*1024*1024;
// seems like we should have a generic set implementation since the
// savings are 1/2 
static buffer world;

#define is_bitstring(__x) ((tagof(__x) == tag_large) || (tagof(__x) == tag_utf8))

// it seems a bit odd to be using an iteration index when we beleive that the
// essential character of functions is immune to any notion of order (unless
// that is explicitly part of the domain of the function (better word), but
// there is still no implied ordering.
//
// but globally - we dont care about the implmentation here until there are
//   user defined iterables...this isn't really so terrible, kind of
//   hoisting the local index into the calling frame (β reduction)

boolean iterate_internal(value m, value *index, value *k, value *v)
{
    if (tagof(m) == tag_map) {
        return iterate_map(m, index, k, v);
    }

    if (tagof(m) == tag_small) {
        u64 i = *(u64 *)index ^ (u64)m;
        if (!i) return false;
        
        u64 b = (u64)__builtin_ffsll(i) - 1;
        *(u64 *)index =  (*(u64 *)index | (1<<b));
        *k = (value)b;
        *v = one;
        return true;
    }

    // we need the string index and the byte index. temporarily packing
    // into a value, but need a better soln, since it limits strings
    // to 512MB
    if (tagof(m) == tag_utf8) {
        buffer s = (buffer)m;
        u64 runei = (*(u64 *)index) >> 32;
        u64 biti = (*(u64 *)index) & ((1ull<<32)-1);
        if (biti < s->length) {
            u8 *b = contentsu8(s) + bytesof(biti);
            u64 len = utf8_length(*b);
            *k = (value)runei;
            // should be a bitstring char not a string
            // otherwsie (x[0]=x) if (len(x) == 1)
            *v = (value)characterof(s, biti);
            *(u64 *)index = ((runei + 1) << 32) | (biti + len);
            return true;
        }
        return false;
    }

    halt("implement iterate", tagof(m));
}

        
// we're going to use this for tables...but i think there are some
// normalization issues w/ zero values...also with trailing bits
static boolean buffer_compare(buffer a, u8 *b, u64 length)
{
    if (a->length != length*8) return false;
    for (int i =0 ; i < length; i++)
        if (contentsu8(a)[i] != b[i])
            return false;
    return true;
}

value vof(u64 count, value v)
{
    // xxx - use vector representation
    value t = allocate_table(count);
    for (u64 i =0; i < count ; i++ )
        table_insert(t, (value)i, v);
    return t;
}

// bytes?
value allocate_utf8(u8 *x, u64 bytes)
{
    value v;
    u64 h = hash_bitstring(x, bitsof(bytes));
    u64 h0 = h;

    // this is a set insertion, if we can right the decomposition here..
    buffer *p;
    // xxx - this is mixing up bytes and values
    // make a set for god sakes and make a slot macro
    while (*(p = (buffer *)contents(world) + (h%world_length))) {
        // xxx - this isn't correct. structural equality doesn't
        // care about the underlying representation
        if ((tagof(*p) == tag_utf8) && (buffer_compare(*(buffer *)p, x, bytes))) 
            return *(value *)p;
        h++; // there are better functions(?)
    }
    
    buffer b = allocate(tag_utf8, bitsof(bytes));
    b->hash = h;
    __builtin_memcpy(contents(b), x, bytes);
    *p = b;
    return b;
}

__attribute__((noreturn)) void halt_internal(char *x, ...)
{
    write(1, x, strlen(x));
    exit(-1);
}

buffer allocate(tag t, bits length)
{
    buffer b = (buffer)((t << tag_offset) | fills[t]);
    b->length = length;
    fills[t] += length + 128; // buffer header - formalize
    return b;    
}

// numbers are little...so that bit 1 is in the first word..that..
// makes sense right?...except they are little forever
buffer buffer_from_number(u64 n)
{
    buffer b = allocate(tag_large, 64);
    b->length = 64;
    memcpy(contents(b), &n, sizeof(u64));
    // we could .. save a couple bytes here...zero the tail
    return b;
}


u64 hash(value v);
u64 hash_map(buffer b);

// if this is cached at the buffer level, then when does this get called?
// only on construction - so we dont need to mux this, do we? cache
// or tag_small!
u64 hash(value v)
{
    if (tagof(v) == tag_map) return hash_map(v);
    if (is_bitstring(v)) {
        buffer b = v;
        return b->hash;
    }
    if (tagof(v) == tag_small) return hash_imm((u64)v, 0);
    halt("unknown tag - detag me please");
}

void out(buffer b)
{
    write(1, contents(b), b->length>>3);
}

value get_small(value v, value k)
{
    if (tagof(k) != tag_small) return 0;
    u64 v64 = (u64)v;
    u64 k64 = (u64)k;
    return toboolean(k64 & (1<<v64));
}

// dervied from tag offset being 32 and the maximum decimal
// representation length...but its not 32!...need a log_base
buffer format_number(value v, int base)
{
    u8 staging [12];
    int fill = 11;
    u64 x = (u64)v;
    while (x) {
        staging[fill--] = (x%base)+'0';
        x /= base;
    }
    buffer b = allocate_utf8(staging+fill+1, 11-fill);
    return b;
}


void output(buffer b)
{
    // ordering!
    foreach(k, v, b) {
        if (tagof(v) != tag_small) {
            halt("output non-char-vector", v);
        }
        u64 x = (u64)v;
        write(1, &x, bytesof(utf8_length(x)));
    }
}

void outputline_internal(void *trash, ...)
{
    buffer b;
    boolean first = true;
    foreach_arg(trash, b) {
        if (!first) write(1, " ", 1);
        first = false;
        output(b);
    }
    write(1, "\n", 1);
}

// could be parameterized by a function if we get there
// m(_k, _v)
// _v = from
// o(_k, _to)

// could save a copy if no matches .. shrug...actually we have to because of
// extensional identity...this does* need to be interned dude
value replace(value m, value from, value to)
{
    value out = allocate_table(nzv(m));
    foreach(k, v, m) {
        if (equals(v, from)) v = to;
        table_insert(out, k, v);
    }
    return out;
}

// that this needs to be different than replace is quite wrong
// also ... we cant actually replace '\n' with '    ' - oh yeah we can
// should be string->string
string replace_string(string m, value from, string to)
{
    value out = allocate_nursery(m->length);
    for_utf8_in_string(i, m) {
        if (equals((value)i, from)) {
            //            if (tagof(to) != tag_utf8) halt("zorn");
            push_mut_vector(out, to);
        } else  { 
            push_mut(out, &i, utf8_length(i));
        }
    }
    return utf8_from_nursery(out);
}

void runtime_init()
{
    void *h = 0;
    for (u64 i=1; i<tag_max; i++) {
        // check error
        void *base = (void *)(i<<tag_offset);
        void *x = mmap((void *)base,
                       1ull<<tag_offset,
                       PROT_READ | PROT_WRITE,
                       MAP_ANON, -1, 0);
        if (x != base) halt("region mmap failure");
    }
    if (!(world = (buffer)allocate(tag_set, world_length * bitsizeof(value))))
        halt("total world failure");
    __builtin_memset(contents(world), 0, world_length * sizeof(value));
}

// what does concat .. mean? only for vectors i think
// right, for bitvectors we can take a zero-extended value and
// coerce it to a vector

value concat_internal(value trash, ...)
{
    int total;
    foreach_arg(trash, b) total += nzv(b);
    // vector rep
    value t = allocate_table(total);
    u64 offset = 0, o2;
    // maybe validate that the inputs are all vector-shaped?
    foreach_arg(trash, b) {
        foreach_ordered(k, v, b) {
            u64 p = (u64)k + offset;
            if ((u64)k < o2) o2 = (u64)k;
            table_insert(t, (value)p, v);
        }
        offset = o2;
    }
    return t;
}

// this is not* table specific..so there..hoist .. oh, also larvation
buffer print_value(value v)
{
    bytes total = 0, indent = 0;

    nursery keys = allocate_nursery(16);
    foreach(k, _, v) {
        buffer kr = print(k);
        u64 klen = bytesof(kr->length);
        // runes should be length(keyr)  - really nzv
        if (klen > indent) indent = klen; // runes not bytes! max not mut!
        push_mut(keys, &kr, bitsizeof(value));
    }

    
    // so these are really something like (apply concat (map x f))
    // +1 for the colon
    string nested_indent = concat(stringify("\n"), vof(indent + 1, (value)' '));

    // dont need to pupate since out is a nursery also
    // if we do - this size is known from above
    nursery values = allocate_nursery(16);
    foreach(_, vi, v) {
        string svi = replace_string(print(vi), (value)'\n', nested_indent);
        total += bytesof(svi->length) + 3;
        push_mut(values, &svi, bitsizeof(value));
    }
    
    // could pupate in target space - avoiding a copy and cementing the nursery
    // as a concept - concat kinda needs this?
    nursery out = allocate_nursery(total);
    boolean first = true;
    forz(k, v, keys, values) {
        //        if (!first) 
        push_mut_vector(out, vof(indent - nzv(*k) + 1, (value)' '));

        first = false;
        push_mut_buffer(out, *k);
        push_mut(out, ":", 8);
        push_mut_vector(out, *v); // assumes string f(n)->utf8
        push_mut(out, "\n", 8);
    }
    if (out->offset) out->offset -= 8;
    return utf8_from_nursery(out);
}


// using tag to drive formatting is hidden state! - we're gonna have to work that
buffer print(value v)
{
    if (v == zero) return sym(0);
    if (tagof(v) == tag_map) return print_value(v);
    if (tagof(v) == tag_utf8) return v;
    if (tagof(v) == tag_large) halt("unsupported large printf support");
    if (tagof(v) == tag_small) return format_number(v, 10);
    halt("bigoo no1tag\n");
}
