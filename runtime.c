#include <stdio.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>
#include <runtime.h>
#include <unistd.h>

u64 fills[tag_max];
u64 lengths[tag_max];


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
    value *region = contents(b);
    u64 hv = hash(k);
    for (int i =0; i < b->length/(entry_size*64) ; i++) {
        region += entry_size;
        if ((region[0] == k) || empty_entry(region)){
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
    if (tagof(v) == tag_large) return hash_ab(v);
    if (tagof(v) == tag_utf8) return hash_ab(v);    
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
    if (tagof(v) == tag_map) return hash_map(v);
    if (tagof(v) == tag_large) return hash_ab(v);
    if (tagof(v) == tag_small) return hash_imm((u64)v, 0);
    halt("unknown tag");
}

#if 0
    
    for (u64 i =0 ;i < 100; i++)
        printf ("%lld -> %llx %llx\n", i,
                hash((void *)i),
                hash_ab(buffer_from_number(i)));

#endif

buffer print_table(value v)
{
    buffer b = v; 
    buffer tags[b->length/64];
    value *k = contents(b);
    u64 total = 0, indent = 0;
    u64 slots = b->length/bitsizeof(u64);

    // (map print (keys v))
    for (int i = 0; i < slots ; i += 2){
        value *k = contents(b) + (i*bitsizeof(value));
        if (!empty_entry(k)) {
            tags[i] = print(k[0]);
            u64 klen = tags[i]->length;
            if (klen > indent) indent = klen;
            total += klen;
            tags[i+1] = print(k[1]);
        } // else slots--;
    }
    
    total += indent * slots + 2 ; // per-line costs, could be less
    buffer out = allocate(tag_utf8, total);
    // (apply concat tags)
    u64 fill = 0;
    for (int i = 0; i < slots ;i+=2){
        value *k = contents(b) + (i*bitsizeof(value));
        if (!empty_entry(k)) {        
            // insert indent 
            memcpy(contents(out) + (fill/8), contents(tags[i]), tags[i]->length/8);
            fill += tags[i]->length;
            memcpy(contents(out) + (fill/8), contents(tags[i+1]), tags[i+1]->length/8);
            fill += tags[i+1]->length;
        }
    }
    return out;
}

buffer format_number(value v, int base)
{
    char staging [10];
    // dervied from tag offset being 32 and the maximum decimal
    // representation length
    int fill = 0;
    u64 x = (u64)v;
    while (x) {
        staging[fill++] = (x%base)+'0';
        x /= base;
    }
    buffer b = allocate(tag_utf8, fill * 8);
    for (; fill > 0 ; fill--) memcpy(contents(b) + fill, staging + fill, 8);
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

void runtime_init()
{
    void *h = 0;
    for (u64 i=1; i<tag_max; i++) {
        void *x = mmap((void *)(i<<tag_offset),
                       1ull<<tag_offset,
                       PROT_READ | PROT_WRITE,
                       MAP_ANON, -1, 0);
         printf("%p\n", x);
    }
}


