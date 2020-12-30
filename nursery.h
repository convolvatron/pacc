// am avoiding putting the discussion here about whether or not this is a
// good model to adopt (larvation)...seems like any other model depends
// on the compiler

#include <stdlib.h>

typedef struct nursery { //ahem
    u8 *resizer;
    bits offset;
    bits length;
} *nursery;

static inline void push_mut(nursery e, void *source, bits length)
{
    if (e->length < e->offset + length)  {
        e->length *= 2;
        e->length += length; // meh
        e->resizer = realloc(e->resizer, e->length);
    }
    __builtin_memcpy(e->resizer + bytesof(e->offset), source, bytesof(length));
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

// vector rep
static inline value vector_from_nursery(nursery n)
{
    value t = allocate_table(n->offset/bitsizeof(value));
    u64 count = 0;
    fornv(i, n) table_insert(t, (value)(count++), i);
    return allocate_utf8(n->resizer, bytesof(n->offset));
}


#define indin(__n, __i)  ((__i)<(value *)(((u8 *)(__n)->resizer)+bytesof((__n)->offset)))

#define forz(__i1, __i2, __n1, __n2)                                    \
    for (value *__i1 = (value *) __n1->resizer, *__i2 = (value *)__n2->resizer; \
         indin(__n1, __i1) && indin(__n2, __i2);                        \
         __i1++, __i2++)

#define push_mut_buffer(__n, __b)\
    push_mut(__n, contentsu8((buffer)__b), ((buffer)__b)->length)

// since these are usually small...ordered iteration!
#define push_mut_vector(__n, __b)\
    foreach(_k, _v, __b) {\
      if (tagof(_v) != tag_small) halt("implement push_kut_vector *");\
      push_mut(__n, &_v, utf8_length((u32)_v));       \
    }


