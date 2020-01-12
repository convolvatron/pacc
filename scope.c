#include <pacc.h>

typedef struct scope *scope;

struct scope {
    struct object o;
    tuple here;
    scope parent;
};
    
value scope_get(scope s, symbol k)
{
    value v;
    if ((v = get(s->here, k))) return v;
    return get(s->parent, k);
}

//void scope_set(scope s, symbol k, value v)
//{
//    set(s->here, k, v);
//}


value sget_internal(tuple t, ...)
{
    value v = t;
    // empty is a valid map
    foreach_arg(t, x) v = get(v, x);
    return v;
}


scope allocate_scope(scope parent)
{
    scope s = allocate(sizeof(struct scope));
    s->o.get = sget_internal;
    //    s->o.set = sget_internal; yes or no?
    s->o.iterate = 0;// is this the local scope, or the union? seems like it has to be the union
    return s;
}
