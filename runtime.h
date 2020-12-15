
typedef void *value;

#define zero ((void *)0)
#define one ((void *)1)
#define INVALID_ADDRESS ((void *)(-1ull))

#define varg  __builtin_va_arg
#define vlist  __builtin_va_list
#define vstart __builtin_va_start
#define vend __builtin_va_end

#define foreach_arg(__start, _x)                        \
    for (void *_x = 0 ; _x != INVALID_ADDRESS; )                        \
        for (__builtin_va_list _va;  (__builtin_va_start(_va, __start)), _x != INVALID_ADDRESS;) \
            for (; _x=__builtin_va_arg(_va, value), _x != INVALID_ADDRESS;)

typedef unsigned long long u64;
typedef unsigned long u32;
typedef unsigned char u8;
typedef struct buffer *buffer;
typedef buffer string;
typedef u64 bits;
typedef u64 bytes;
typedef value boolean;
#define toboolean(__x) ((void *)((__x)?true:false))
#define true one
#define false zero

#define bitsof(_x) ((_x) << 3)
#define bytesof(_x) ((_x) >> 3)
#define bitsizeof(_x) (sizeof(_x)*8)

string print(value);

static inline u64 hash_imm(u64 x, u64 offset)
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

#include <stdio.h>

//deprintify
__attribute__((noreturn)) void halt_internal(char *message, ...);
#define halt(...) {\
    printf("halt in [%s:%s:%d]\n", __FILE__, __FUNCTION__, __LINE__); \
    halt_internal(__VA_ARGS__, INVALID_ADDRESS);}

value allocate_utf8(u8 *x, u64 bytes);
#include <buffer.h>

void runtime_init();

typedef u64 tag;

#define tag_offset 36ull
#define tag_small 0ull
#define tag_map 1ull
#define tag_large 2ull
#define tag_utf8 3ull
#define tag_set 4ull
#define tag_union 5ull
#define tag_max 6ull
#define tagof(__x) ((u64)(__x) >> tag_offset)

buffer allocate(tag t, bits length);
boolean iterate_map(value m, value *index, value *k, value *v);
buffer allocate_table(int entries);

static inline value stringify(char *x)
{
    int total = 0;
    while (x[total++]);
    return(allocate_utf8((u8 *)x, total-1));
}

#define sym(_x) stringify(#_x)

void table_insert(buffer t, value k, value v);
value table_get(value t, value k);

void output(buffer b);
void outputline_internal(void *trash, ...);
#define outputline(...) outputline_internal(0, __VA_ARGS__, INVALID_ADDRESS);

static inline value timm_internal(value trash, ...)
{
    int total = 0;                              
    foreach_arg(trash, i) total++;
    if (total &1) {halt("key without value in timm"); } 
    buffer t = allocate_table(total/2);
    value key;
    foreach_arg(trash, i) {
        if (total++&1) {
            table_insert(t, key, i);
        } else {
            key = i;
        }
    }
    return t;
}

#define timm(...) timm_internal(0, __VA_ARGS__, INVALID_ADDRESS)


value get_small(value v, value k);

static inline value get(value v, value k)
{
    if (v == zero) return zero;
    
    if (tagof(v) == tag_map)
        return table_get(v, k);

    if (tagof(v) == tag_small)
        return get_small(v, k);

    // oh...you...
    if (tagof(v) == tag_utf8) {
        // 4g runes
        buffer s = v;
        if (tagof(k) == tag_small) return false; // xx or large
        u64 start = 0;
        for (u64 ind=0, bit =0; ind < (u64)k;
             ind++, bit += utf8_length(*contentsu8(s)+bytesof(start)));
        return substring(s, start, start+utf8_length(*contentsu8(s)+bytesof(start)));
    }
            
    if (tagof(v) == tag_union){
        value i;
        scan_buffer(i, (buffer)v, bitsizeof(value), value) {
            value z = get(*(value*)i, k);
            if (z) return z;
        }
    }
    halt("bad lookup tag");
}

boolean iterate_internal(value m, value *index, value *k, value *v);
#define foreach(__k, __v, __f)\
    for(value __k, __v, __ind = 0;iterate_internal(__f, &__ind, &__k, &__v);) 


static inline boolean equals(value a, value b)
{
    u64 count = 0;
    if (a == b) return true;
    foreach(ka, va, a) {
        if (va != get(b, ka)) return false;
        count++;
    }
    // we are trying to avoid committing to length, so we are doing another
    // order n. if foreach is defined, then length is defined..so..
    foreach(ka, va, a) count--;
    
    return toboolean(count == 0);
}

// fix i guess
static inline u64 nzv(value v)
{
    u64 count = 0;
    foreach(_1, _2, v) count++;
    return count;
}

buffer print_value(value v);
