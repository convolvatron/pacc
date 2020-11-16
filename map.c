#include <runtime.h>

#define entry_size 2 // (key, value)
extern u64 hash(value);

#define empty_entry(__x) (__x[1] == 0)
#define table_len(__t)  ((__t)->length/2*bitsizeof(value))
#define slot(__t, __i)  ((value *)contents(__t) + 2*((h)%table_len(__t)))

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
    u64 hv = hash(k);
    fort(region, b) {
        if ((region[0] == k) || empty_entry(region)){
            region[0] = k;            
            region[1] = v;
            return;
        }
    }
    halt("table overflow");
}

value table_get(value t, value k)
{
    buffer b = t;
    u64 h = b->hash;
    int tlen = table_len(b);
    int count = 0;
    value *p;
    
    while ((*(p = slot(b, h)) && (count++ < tlen)))
            if (p[0] == k) return p[1];
    return 0;
}

#define paste(__out, __fill, __source, __bits) {\
    __builtin_memcpy((u8 *)contents(__out) + (__fill/8), __source, __bits/8); \
   __fill += __bits;\
}

buffer print_table(value v)
{
    buffer b = v; 
    buffer tags[b->length/64];
    value *k;
    bytes total = 0, indent = 0;

    int i=0;
    fort(k, b) {
        if (!empty_entry(k)) {
            buffer keyr = print(k[0]);
            buffer valr = print(k[1]);
            
            u64 klen = keyr->length/8;
            if (klen > indent) indent = klen; // runes not bytes!
            
            total += valr->length/8 + indent + 2;
            tags[i++] = keyr;
            tags[i++] = valr;            
        }
    }
    
    // (apply concat tags)
    buffer out = allocate(tag_utf8, total * 8);
    u64 fill = 0;
    i=0;
    fort(k, b) {
        if (!empty_entry(k)) {
            paste(out, fill, contents(tags[i]), tags[i]->length);
            for (int sp = 0; sp < indent-(tags[i]->length>>3); sp++)
                paste(out, fill, " ", 8);
            paste(out, fill, ":", 8);
            i++;
            paste(out, fill, contents(tags[i]), tags[i]->length);
            i++;            
            paste(out, fill, "\n", 8);
        }
    }
    return out;
}
