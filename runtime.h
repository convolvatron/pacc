#define varg  __builtin_va_arg
#define vlist  __builtin_va_list
#define vstart __builtin_va_start
#define vend __builtin_va_end

typedef void *value;

#define zero ((void *)0)
#define one ((void *)1)

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
typedef value boolean;
#define toboolean(__x) ((void *)((__x)?true:false))
#define true one
#define false zero


#define contents(__x) ((void *)&(__x)->contents)
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
#define tag_max 4ull
#define tagof(__x) ((u64)(__x) >> tag_offset)

buffer allocate(tag t, bits length);

buffer allocate_table(int entries);
    
#define sym(_x) ({\
    int len = strlen(#_x);\
    buffer b = allocate(tag_utf8, len*8);\
    __builtin_memcpy(contents(b), #_x, len);\
    b;\
})

#define symq(_x) false


// see if its in the object table - is the 2hp hash-o-matic big enough for this?
static inline value stringify(char *x)
{
    int total = 0;
    while (x[total++]);
    buffer b = allocate(tag_utf8, total*8);
    __builtin_memcpy(contents(b), x, total);
    return b;
}

void table_insert(buffer t, value k, value v);

buffer print(value);
