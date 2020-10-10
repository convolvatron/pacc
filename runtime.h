#define varg  __builtin_va_arg
#define vlist  __builtin_va_list
#define vstart __builtin_va_start
#define vend __builtin_va_end

typedef void *value;

#define foreach_arg(__start, _x)                                       \
    for (__builtin_va_list _va; __builtin_va_start(_va, __start), 1; ) \
        for (void *_x;  _x=__builtin_va_arg(_va, value), _x != INVALID_ADDRESS; )

typedef unsigned long long u64;

typedef struct heap *heap;
typedef struct buffer *buffer;
typedef void *value;
typedef buffer string;
typedef void *symbol;
typedef struct table *tuple;
typedef tuple vector;
typedef unsigned char u8;
typedef string character;

typedef u64 bits;

typedef struct buffer {
    bits start, end, length;
    u8 *contents;
} *buffer;
    

typedef value boolean;

// maybe this is all unifiable?
typedef struct object {
    value (*get)(value, value);
    void (*iterate)(value, value*, value*);
    u64 (*length)(value);
}*object;
    
#define true ((void *)1)
#define false ((void *)0)

#define sym(_x) ((void *)0)
string aprintf(char *format, ...);
string bprintf(string s, char *format, ...);

typedef void *value;
buffer allocate_buffer();
#define foreach(__k, __v, __t)\
    for(void *__k, *__v; ;)

buffer append(buffer, string);
#define get(first, ...)  ((void *)0)

#define toboolean(__x) ((void *)((__x)?true:false))

void vector_push(tuple, value);
#define symq(__x) ((void *)0)

#define set(__t, __n, __v) ((void *)0)
#define size(__t) 0

#define u64_from_value(__x) 0
#define value_from_u64(__x) ((void *)0)

// no mutagens
void vector_push(vector v, value x);
value vector_pop(vector v);
typedef u64 bytes;
void *allocate(bytes size);

typedef char s8;

static inline s8 digit_of(character z)
{
    u8 x = *(u8 *)(z->contents + z->start); 
    if ((x <= 'f') && (x >= 'a')) return(x - 'a' + 10);
    if ((x <= 'F') && (x >= 'A')) return(x - 'A' + 10);
    if ((x <= '9') && (x >= '0')) return(x - '0');
    return(-1);
}

// ignoring all the other valid alphas...like alpha
static inline boolean isalpha(character z)
{
    u8 x = *(u8 *)(z->contents + z->start); 
    if ((x <= 'z') && (x >= 'a')) return true;
    if ((x <= 'Z') && (x >= 'A')) return true;
    return false;
}

void push(value, value);
typedef struct scope *scope;
scope allocate_scope(scope);
value cstring();
