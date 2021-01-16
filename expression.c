#include <pacc.h>

result leaf(parser p, u64 offset, scope env, u64 left_precedence);

result middle(parser p, u64 offset, scope env, u64 left_precedence, value left)
{
    // ok, i've being called with p=5, and the new guy is p=3,
    // so i want to reduce the left....i dont have to do any
    // funny caching or flow control since this is functional(?)
    // so i need a different result (?) so he can reduce and call
    // me again?

    value t = token(p, offset);
    value op = pget(env, sym(operators), sym(binary), get(t, sym(value)));
    if (!op) return failure(stringify("operator not found"), offset);
    result right =  leaf(p, offset+1, env, (u64)get(op, sym(precendence)));
    return res(timm(sym(right), right.v,
                    sym(left), left,
                    sym(operator), op),               
               right.offset);
}

static inline boolean terminal(value t)
{
    return true;
}

// ok - instead of capturing a closure to perform the reduction we think we're
// going to evaluate middle twice? - what are the complexity implications?

result leaf(parser p, u64 offset, scope env, u64 left_precedence)
{
    value t = token(p, offset);
    if (terminal(t)) {
        // need to combine this term with the leftmost?
        result right = middle(p, offset+1, env, left_precedence, t);
        if (isfailure(right)) return res(t, offset+1);
        return right;
    } else {
        // prefix case
        value op = pget(env, sym(prefix), get(t, sym(value)));
        result right = leaf(p, offset+1, env, (u64)get(op, sym(precedence)));
        return right;
    }
}

                         
result read_expression(parser p, u64 offset, scope env)
{
    result z = leaf(p, offset, env, 0);
    printf("result: [%s %p %lld %p]", z.success?"true":"false", z.v, z.offset, z.env);
    //    outputline(sym("pag"), print(z.v));
    output(emit_expression(z.v));
    halt("zig");
    return z;
}

result read_cast_expression(parser p, u64 offset, scope env)
{
    return res(zero, 0);
}

