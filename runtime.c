#include <stdio.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>
#include <runtime.h>

u64 fills[tag_max];
u64 lengths[tag_max];


__attribute__((noreturn)) void halt()
{
    exit(-1);
}

#define bitsizeof(_x) (sizeof(_x)*8)
#define contents(__b) ((void *)(&(b)->contents))

static buffer allocate(tag t, bits length)
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
        working ^= (1<<k);
    }
    return out<<1;
}

// need to get 1 bits set
// doesn't work for > 64 bits..umm, yes...up to 2^64?
u64 hash_ab(buffer b)
{
    u64 out = 0, k;

    for (int offset = 0; offset < b->length ; offset += bitsizeof(u64)) {
        // are we going to pad to 8 bytes to get these zeros or mask?
        u64 k = ((u64 *)contents(b))[offset>>8];
        out ^= hash_imm(k, offset);
    }
    // multiple words match w/ out<<1? above?
    return out;
}

#define entry_size 2 // (key, value)

// we would just _never_ insert a value of zero
#define empty_entry(__x) (__x[1] == 0)

u64 hash(value v);

u64 hash_map(buffer b)
{
    u64 result = 0;
    value *region = contents(b);
    for (int offset; offset < b->length ; offset += entry_size) {
        if (!empty_entry(region)) 
            result ^= hash(region[1]) ^ hash(region[2]);
        region += entry_size;
    }
    return result;
}


// assuming that k and v have both been interned.
void table_insert(buffer b, value k, value v)
{
    value *region = contents(b);
    u64 hv = hash(k);
    for (int i =0; i < b->length/(entry_size*64) ; i++) {
        region += entry_size;
        if ((region[0] == k) || empty_entry(region)){
            region[1] = v;
        }
    }
    halt();
}


// if this is cached at the buffer level, then when does this get called?
// only on construction - so we dont need to mux this, do we? cache
// or tag_small!
u64 hash(value v)
{
    if (tagof(v) == tag_map) return hash_map(v);
    if (tagof(v) == tag_large) return hash_ab(v);
    if (tagof(v) == tag_small) return hash_imm((u64)v, 0);
    halt();
}

value get_small(value v, value k)
{
    u64 v64 = (u64)v;
    u64 k64 = (u64)k;
    return toboolean(k64 & (1<<v64));
}

// if this is cached at the buffer level, then when does this get called?
u64 get(value v)
{
    if (tagof(v) == tag_map) return hash_map(v);
    if (tagof(v) == tag_large) return hash_ab(v);
    if (tagof(v) == tag_small) return hash_imm((u64)v, 0);
    halt();
}

int main()
{
    void *h = 0;
    for (u64 i=1; i<tag_max; i++) {
        void *x = mmap((void *)(i<<tag_offset),
                       1ull<<tag_offset,
                       PROT_READ | PROT_WRITE,
                       MAP_ANON, -1, 0);
         printf("%p\n", x);
    }
    
    for (u64 i =0 ;i < 100; i++)
        printf ("%lld -> %llx %llx\n", i,
                hash((void *)i),
                hash_ab(buffer_from_number(i)));
}


