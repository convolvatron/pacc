#include <pacc.h>

// world contains the left and right relation..would love to have
// some kind of more convenient support for inverse maps...really
// the compiler should push the inverse reference to the setters (decidability)

// ok, we're using the presence of precedence to determine an operator

#define p(__x) ((u64)get(__x, sym(precedence)))

static value expression_reduce(value right, value left, value x)
{
    // should return result with the max lexical position so
    // the linear machine can start up again
    if (!nzv(right)) return x;
    
    if (pget(x, sym(operator))) {
        value r= get(right, x);
        value l= get(left, x);
        value ro= get(left, r);
        value lo= get(left, r);
        if ((p(x) > p(ro)) && (p(x) > p(lo))) {
            value new = timm(sym(compound), 5, sym(left), l, sym(right), r);
            // need 4 new adjacency facts..what about the ends? that works out too? except
            // we end up with zero->node?
            return expression_reduce(combine(timm(new, ro), timm(new, lo), right),
                                     combine(timm(new, lo), timm(new, ro), left),
                                     p(ro)>p(lo)?ro:lo);
        }
    }
    return x;
}

                         
result read_expression(parser p, u64 offset, scope env)
{
    return res(zero, 0);
}

result read_cast_expression(parser p, u64 offset, scope env)
{
    return res(zero, 0);
}

