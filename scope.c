#include <pacc.h>

struct scope {
    struct object o;
    scope parent;
    u64 size;
    value elements[]; 
};
    
value scope_get(value z, value k)
{
    scope s = z;
    value v;
    if ((v = get(s->here, k))) return v;
    return get(s->parent, k);
}

//void scope_set(scope s, symbol k, value v)
//{
//    set(s->here, k, v);
//}


value sget_internal(value v, ...)
{
    //scope s = v;
    // empty is a valid map
    foreach_arg(v, x) v = get(v, x);
    return v;
}


scope allocate_scope(scope parent)
{
    scope s = allocate(sizeof(struct scope));
    s->o.get = scope_get;
    //    s->o.set = sget_internal; yes or no?
    s->o.iterate = 0;// is this the local scope, or the union? seems like it has to be the union
    return s;
}

