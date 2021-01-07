#include <runtime.h>

#define entry_size 2 // (key, value)
extern u64 hash(value);

#define empty_entry(__x) ((__x)[1] == 0)
#define table_len(__t)  ((__t)->length/(entry_size*bitsizeof(value)))
#define slot(__t, __i)  ((value *)contents(__t) + entry_size*((__i)%table_len(__t)))

#define fort(__i, __t)\
    for (void **__i = contents(__t), **__end = __i+(( __t)->length>>6); __i<__end; __i+=2)

u64 hash_map(buffer b)
{
    u64 result = 0;
    fort(p, b) {
        if (!empty_entry(p)) 
            result ^= hash(p[1]) ^ hash(p[2]);
        p += entry_size;
    }
    return result;
}

buffer allocate_table(int count)
{
    int bytes = ((3 * count) / 2) * entry_size * sizeof(value);
    buffer b = allocate(tag_map, bytes*8);
    __builtin_memset(contents(b), 0, bytes);
    return b;
}
        
// assuming that k and v have both been interned.
void table_insert(buffer b, value k, value v)
{
    int count = 0; // really shouldn't happen
    for (u64 hv = hash(k); count < table_len(b) ;hv++) {
        value *s = slot(b, hv);
        // no muts here!! we're allowing zero because its convenient - 
        if (s[0] && s[1] && equals(s[0], k)) halt("larval table overwrite");
        if (empty_entry(s)){
            s[0] = k;            
            s[1] = v;
            return;
        }
    }
    halt("table overflow");
}

value table_get(value t, value k)
{
    buffer b = t;
    u64 h = hash(k);
    int tlen = table_len(b);
    int count = 0;
    value *p;

    while (!empty_entry(p = slot(b, h++)) && (count++ < tlen))
        if (equals(p[0], k)) return p[1];
    return 0;
}


boolean iterate_map(value m, value *index, value *k, value *v)
{
    value *t;
    // this can be easier
    while ((t = (value *)(contentsu8((buffer)m))  + *((u64 *)index)),
           *index += entry_size,
           t < (value *)(contentsu8((buffer)m) + bytesof(((buffer)m)->length))){
        if (!empty_entry(t)) {
            *k = t[0];
            *v = t[1];
            return true;
        }
    }
    return false;        
}

