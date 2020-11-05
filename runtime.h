#define varg  __builtin_va_arg
#define vlist  __builtin_va_list
#define vstart __builtin_va_start
#define vend __builtin_va_end

typedef void *value;

#define zero ((void *)0)
#define one ((void *)1)


#define foreach_arg(__start, _x)                                       \
    for (__builtin_va_list _va; __builtin_va_start(_va, __start), 1; ) \
        for (void *_x;  _x=__builtin_va_arg(_va, value), _x != INVALID_ADDRESS; )

typedef unsigned long long u64;
typedef unsigned long u32;
typedef unsigned long u8;
typedef struct buffer *buffer;
typedef buffer character;
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
    u64 hash;
    u8 contents[];
} *buffer;


typedef u64 tag;

#define tag_offset 36ull
#define tag_small 0ull
#define tag_map 1ull
#define tag_large 2ull
#define tag_utf8 3ull
#define tag_max 4ull
#define tagof(__x) ((u64)(__x) >> tag_offset)

buffer allocate(tag t, bits length);


#define sym(_x) false
#define symq(_x) false

