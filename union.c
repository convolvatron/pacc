#include <pacc.h>

// ok, we need to sort out foreach - i guess either flatten up front
// or an emitted list..the second saves the size of the last table?
// kinda depends on the size?

#define forv(__i, __t)\
    for (value *__i = contents(__t), *__end = __i+(( __t)->length>>6); __i<__end; __i+=1)

value get_union(value m, value k)
{
    value v;    
    forv(i, (buffer)m) if ((v = get(*i, k))) return v;
    return zero;
}



