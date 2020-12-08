
typedef void *value;

#define zero ((void *)0)
#define one ((void *)1)

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

#define contents(__x) ((void **)&(__x)->contents)
#define contents64(__x) ((u64 *)&(__x)->contents)
#define contentsu8(__x) ((u8 *)&(__x)->contents)
#define length(__x) ((void *)&(__x)->length)

typedef struct buffer {
    u64 length;
    u64 hash; // ?  
    u8 contents[];
} *buffer;

void runtime_init();

#include <stdio.h>

#define INVALID_ADDRESS ((void *)(-1ull))

//deprintify
__attribute__((noreturn)) void halt_internal(char *message, ...);
#define halt(...) {\
    printf("halt in [%s:%s:%d]\n", __FILE__, __FUNCTION__, __LINE__); \
    halt_internal(__VA_ARGS__, INVALID_ADDRESS);}

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

buffer allocate_table(int entries);

u64 hash_bitstring(u8 *x, u64 bytes);

value allocate_utf8(u8 *x, u64 bytes);

static inline value stringify(char *x)
{
    int total = 0;
    while (x[total++]);
    return(allocate_utf8((u8 *)x, total-1));
}

#define sym(_x) stringify(#_x)

void table_insert(buffer t, value k, value v);

// maybe a star?
static inline bits utf8_length(u32 x)
{
    if (~x & 0x80) return 8;
    if ((x & 0xe0) == 0xc0) return 16;
    if ((x & 0xf0) == 0xe0) return 24;
    if ((x & 0xf8) == 0xf0) return 32;
    halt("invalid utf8 character");
}

buffer print(value);
void output(buffer b);

value table_get(value t, value k);


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

// ^ fort
#define scan_buffer(__i, __t, __stride, _ty)\
    for (void *__j = contents(__t), *__end = __j+(( __t)->length>>6); (__i = __j), (__j<__end); __j += __stride/8)

value get_small(value v, value k);

static inline buffer substring(buffer b, bits start, bits end)
{
    return allocate_utf8(contentsu8(b)+(start>>3), bytesof(end-start));
}

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

typedef u32 character; 


// we know that a small is at least a u32 and a u32 can represent
// all utf8 codepoints
static inline u32 characterof(buffer b, bits offset)
{
    u32 x = 0;
    u8 *p = contentsu8(b) + bytesof(offset);
    int len = utf8_length(*p);
    __builtin_memcpy(&x, p, bytesof(len));
    return x;
}


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





