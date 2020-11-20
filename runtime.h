
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

__attribute__((noreturn)) void halt(char *message);

typedef u64 tag;

#define tag_offset 36ull
#define tag_small 0ull
#define tag_map 1ull
#define tag_large 2ull
#define tag_utf8 3ull
#define tag_set 4ull
#define tag_max 5ull
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

buffer print(value);
void output(buffer b);

value table_get(value t, value k);

#define INVALID_ADDRESS ((void *)(-1ull))

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
