#include <stdio.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>
#include <runtime.h>
#include <unistd.h>

u64 fills[tag_max];
u64 lengths[tag_max];
u64 wlen = 32*1024*1024;
// seems like we should have a generic set implementation since the
// savings are 1/2 
static buffer world;

#define is_bitstring(__x) ((tagof(__x) == tag_large) || (tagof(__x) == tag_utf8))


// we're going to use this for tables...but i think there are some
// normalization issues w/ zero values...also with trailing bits
static boolean buffer_compare(buffer a, u8 *b, u64 length)
{
    if (a->length != length*8) return false;
    for (int i =0 ; i < length; i++)
        if (contents64(a)[i] != b[i])
            return false;
    return true;
}

value allocate_utf8(u8 *x, u64 bytes)
{
    value v;
    u64 h = hash_bitstring(x, bytes*8);

    // this is a set insertion, if we can right the decomposition here..
    buffer *p;
    while (*(p = (buffer *)contents(world) + (h%wlen))) {
        // this isn't correct. structural equality doesn't
        // care about the underlying representation
        if (tagof(*p) == tag_utf8) {
            if (buffer_compare(*(buffer *)p, x, bytes))
                return *(value *)p;
        }
        h++; // there are better functions(?)
    }
    
    buffer b = allocate(tag_utf8, bytes*8);
    b->hash = h;
    __builtin_memcpy(contents(b), x, bytes);
    printf(" %p %llx\n", b, h);
    output(b);
    *p = b;
    return b;
}

__attribute__((noreturn)) void halt(char *x)
{
    write(1, x, strlen(x));
    exit(-1);
}

#define bitsizeof(_x) (sizeof(_x)*8)

buffer allocate(tag t, bits length)
{
    buffer b = (buffer)((t << tag_offset) | fills[t]);

    b->length = length;
    fills[t] += length;
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


u64 hash_imm(u64 x, u64 offset)
{
    u64 working = x;
    u64 out = 0, k;
    if (x < 2) return x + 1;
    while ((k = (__builtin_ffsll(working)))) {
        k = k - 1;
        out += hash_imm(k + offset, 0); 
        working ^= (1ull<<k);
    }
    return out<<1;
}

// need to get 1 bits set
// doesn't work for > 64 bits..umm, yes...up to 2^64?
// doesn't work for < 64 bits .. we were going to assume padding?... doesn't
// help for cstrings
u64 hash_bitstring(u8 *x, u64 bits)
{
    u64 out = 0, k;

    for (int offset = 0; offset < bits ; offset += bitsizeof(u64)) {
        u64 k = ((u64 *)x)[offset>>8];
        int total = offset - bits ;
        if (total < 64) k &= ((1<< total)-1); //endian?
        out ^= hash_imm(k, offset);
    }
    // multiple words match w/ out<<1? above?
    return out;
}

#define entry_size 2 // (key, value)

// we would just _never_ insert a value of zero
#define empty_entry(__x) (__x[1] == 0)

#define fort(__i, __t)\
    for (void **__i = contents(__t), **__end = __i+(( __t)->length>>6); __i<__end; __i+=2)

u64 hash(value v);

u64 hash_map(buffer b)
{
    u64 result = 0;
    fort(region, b) {
        if (!empty_entry(region)) 
            result ^= hash(region[1]) ^ hash(region[2]);
        region += entry_size;
    }
    return result;
}

buffer allocate_table(int count)
{
    int bytes = ((3 * count) / 2) * entry_size * sizeof(value);
    buffer b = allocate(tag_map, bytes*8);
    bzero(contents(b), bytes);
    return b;
}
        
// assuming that k and v have both been interned.
void table_insert(buffer b, value k, value v)
{
    u64 hv = hash(k);
    fort(region, b) {
        if ((region[0] == k) || empty_entry(region)){
            /*            output(sym(insert));
                          output(stringify(" "));            
                          output(k);
                          output(stringify("\n"));            */
            
            region[0] = k;            
            region[1] = v;
            return;
        }
    }
    halt("table overflow");
}


// if this is cached at the buffer level, then when does this get called?
// only on construction - so we dont need to mux this, do we? cache
// or tag_small!
u64 hash(value v)
{
    if (tagof(v) == tag_map) return hash_map(v);
    if (is_bitstring(v)) {
        buffer b = v;
        return hash_bitstring((u8 *)contents(b), b->length);
    }
    if (tagof(v) == tag_small) return hash_imm((u64)v, 0);
    halt("unknown tag - detag me please");
}

void out(buffer b)
{
    write(1, contents(b), b->length>>3);
}

// method set in objects?
value get_small(value v, value k)
{
    u64 v64 = (u64)v;
    u64 k64 = (u64)k;
    return toboolean(k64 & (1<<v64));
}

// if this is cached at the buffer level, then when does this get called?
// do we really need (or even want in the limit) the tag? isn't this just the
// get an iterate methods?
u64 get(value v)
{
    return 0;
}

#define paste(__out, __fill, __source, __length) {\
   memcpy(contents(__out) + (__fill/8), __source, __length/8);\
   __fill += __length;\
}

buffer print_table(value v)
{
    buffer b = v; 
    buffer tags[b->length/64];
    __builtin_memset(tags, 0, b->length/8);
    value *k = contents(b);
    u64 total = 0, indent = 0;
    u64 slots = b->length/bitsizeof(u64);

    // (map print (keys v))

    int i = 0;
    fort(k, b) {
        if (!empty_entry(k)) {
            tags[i] = print(k[0]);
            u64 klen = tags[i]->length;
            if (klen > indent) indent = klen;
            total += klen;
            tags[i+1] = print(k[1]);
            i +=2 ;
        } else slots-=2;
    }
    
    total += indent * slots + 2 ; // per-line costs, could be less
    buffer out = allocate(tag_utf8, total);
    // (apply concat tags)
    u64 fill = 0;
    i=0;
    fort(k, b) {
        if (!empty_entry(k)) {
            // insert indent
            paste(out, fill, contents(tags[i]), tags[i]->length);
            paste(out, fill, ":", 8);
            paste(out, fill, contents(tags[i+1]), tags[i+1]->length);
            paste(out, fill, "\n", 8);            
        }
        i++;
    }
    return out;
}

buffer format_number(value v, int base)
{
    char staging [10];
    // dervied from tag offset being 32 and the maximum decimal
    // representation length...but its not 32!
    int fill = 0;
    u64 x = (u64)v;
    while (x) {
        staging[fill++] = (x%base)+'0';
        x /= base;
    }
    fill--;
    buffer b = allocate(tag_utf8, fill * 8);
    for (u8 *x = (u8 *)contents(b); fill > 0 ; fill--, x++)
        *x = staging[fill];
    return b;
}

buffer print(value v)
{
    if (tagof(v) == tag_map) return print_table(v);
    if (tagof(v) == tag_utf8) return v;
    if (tagof(v) == tag_large) halt("unsupported large printf support");
    if (tagof(v) == tag_small) return format_number(v, 10);
    halt("bigoo notag\n");
}

void output(buffer b)
{
    write(1, contents(b), b->length/8);
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
    if (!(world = (buffer)allocate(tag_set, wlen)))
        halt("total world failure");
    __builtin_memset(contents(world), 0, wlen);
}


