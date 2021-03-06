// am avoiding putting the discussion here about whether or not this is a
// good model to adopt (larvation)...seems like any other model depends
// on the compiler

#include <stdlib.h>

typedef struct nursery { //ahem
    u8 *resizer;
    bits offset;
    bits length;
} *nursery;

static void nursery_set(nursery n, u64 offset, void *source, bits length)
{
    if (n->length < offset + length)  {
        n->length *= 2;
        n->length += length; // meh
        n->resizer = realloc(n->resizer, n->length);        
    }
    __builtin_memcpy(n->resizer + bytesof(offset), source, bytesof(length));    
}

static inline void push_mut(nursery e, void *source, bits length)
{
    nursery_set(e, e->offset, source, length);
    e->offset += length;
}


static inline nursery allocate_nursery(bits size)
{
    nursery e = malloc(sizeof(struct nursery));
    e->offset = 0;
    e->length = 128;    
    e->resizer = malloc(e->length);
    return e;
}


static inline value utf8_from_nursery(nursery n)
{
    return allocate_utf8(n->resizer, bytesof(n->offset));
}


#define fornv(__v, __n)\
    for (value *__v = (value *)__n->resizer; __v  < ((value *)(__n->resizer + bytesof(n->offset))); __v ++)

static inline value set_from_nursery(nursery n)
{
    value output = allocate_table(n->offset/bitsizeof(value));
    fornv(v, n) table_insert(output, *v, one);
    return output;
}


// vector rep
static inline value vector_from_nursery(nursery n)
{
    value t = allocate_table(n->offset/bitsizeof(value));
    u64 count = 0;
    fornv(i, n) table_insert(t, (value)(count++), i);
    return allocate_utf8(n->resizer, bytesof(n->offset));
}

static inline value table_from_nursery(nursery n)
{
    value t = allocate_table(n->offset/bitsizeof(value));
    u64 parity = 0;
    value k = 0;
    fornv(i, n){
        if (!parity) k = *i;
        else table_insert(t, k, *i);
        parity ^= 1;
    }
    return t;
}


#define indin(__n, __i)  ((__i)<(value *)(((u8 *)(__n)->resizer)+bytesof((__n)->offset)))

#define forz(__i1, __i2, __n1, __n2)                                    \
    for (value *__i1 = (value *) __n1->resizer, *__i2 = (value *)__n2->resizer; \
         indin(__n1, __i1) && indin(__n2, __i2);                        \
         __i1++, __i2++)

#define push_mut_buffer(__n, __b) push_mut(__n, contentsu8((buffer)__b), ((buffer)__b)->length)
// ampersand? 
#define push_mut_value(__n, __v)  push_mut(__n, &__v, bitsizeof(value))

// since these are usually small...ordered iteration!
#define push_mut_vector(__n, __b)\
    foreach(_k, _v, __b) {\
      if (tagof(_v) != tag_small) halt("implement push_kut_vector *");\
      push_mut(__n, &_v, utf8_length((u32)_v));       \
    }



// nursery violation, complexity nightmare
static inline boolean nursery_value_find(nursery n, value v)
{
    fornv(i, n) if (equals(*i, v)) return true;
    return false;
}

