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
    for (value *__v = __n->resizer; __v  < __n->resizer + n->fill; __v ++)

