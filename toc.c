#include "pacc.h"

typedef string formatter(value);

string format_integer(value v, u64 base)
{
    nursery n = allocate_nursery(8);
    // inverse
    return utf8_from_nursery(n);
}

// interstitial or terminal separator? 
string format_array(value v, formatter f, string separator)
{
    return zero;
}
                         
// just keep this in the type
static string string_from_type(Type ty) {

    string kind = get(ty, sym(kind));
    if (kind == sym(ptr))
        return concat(stringify("*"), string_from_type(get(ty, sym(pointsto))));
    
    if (kind == sym(array))                 
        return concat(stringify("["),
                      get(ty, sym(length)),
                      stringify("]"),
                      string_from_type(get(ty, sym(pointsto))));
    
    if ((kind == sym(union)) || (kind == sym(struct)))  {
        value fields;
        //        symbol key = intern(aprintf(transient, "%p", ty));
        if ((fields = get(ty, sym(fields))))
            return concat(stringify("{"),
                          format_array(fields, string_from_type, stringify("")), 
                          stringify("}"));
    }
    
    if (kind == sym(func)) {
        value parameters;
        if (get(ty, sym(parameters))) {
            return concat(stringify("("),
                          format_array(parameters, string_from_type, stringify("")),
                          stringify(")"));
        }
    }
    return kind;
}

string node2s(Node x)
{
    string k = get(x, sym(kind));
    string n = get(x, sym(name));
    value v = get(x, sym(value));
    
    // maka inverse
    value inverse_character_map;

    if (k == sym(literal)) {
        string ntk = pget(n, sym(type), sym(kind));
        if (ntk == sym(char)) {
            value i = get(n, sym(value));
            value x = get(inverse_character_map, i);
            return concat(stringify("'"),
                          x?stringify("\\"):zero,
                          x?x:i,
                          stringify("'"));
        }
        // we can do better here
        if ((ntk == sym(int)) ||
            (ntk == sym(long)) ||
            (ntk == sym(llong))) {
            return format_integer(v, 10);
        }
        if (ntk == sym(array)) {
            return concat(stringify("'"),
                          x?stringify("\\"):zero,
                          x?x:v,
                          stringify("'"));
        }
    }
    // this should just be a property on another node
    if (k == sym(label))   return concat(v, stringify(":"));

    //    if (kind ==  sym(variable)){
    //        bprintf(b, "lv=%s", name);
    //        value init = get(n, sym(lvarinit));
    //        if (init) {
    //            bprintf(b, "(");
    //            a2s_declinit(b, init);
    //            bprintf(b, ")");
    //        }
    //    }
    
    value nty = get(n, sym(type));
    // xx why are these really so different, isn't this just
    // an expression?
    if (k == sym(apply)) 
        concat(stringify("("),
               format_array(get(n, sym(arguments)), node2s, stringify(",")),
               stringify(")"));
    

    // just a cfg edge
    if (k ==  sym(goto)) return concat(sym(goto), sym(";"));
    
    //    if (kind ==  sym(decl)){
    //        bprintf(b, "(decl %b %b",
    //                string_from_type(get(n, sym(declvar), sym(type))),
    //                get(n, sym(declvar), sym(name)));
    //        if ( get(n, sym(declinit))){
    //            bprintf(b, space);
    //            a2s_declinit(b, get(n, sym(init)));
    //        }
    //        bprintf(b, close_paren);
    //        return;
    //    }
    
    if (k ==  sym(if)) {
        return concat(sym(if),
                      stringify("("),
                      node2s(get(n, sym(cond))),
                      stringify(")"),                      
                      sym(then),
                      node2s(get(n, sym(then))),
                      sym(else), // maybe
                      node2s(get(n, sym(else))));
    }

    // positional?
    if (k ==  sym(ternary)) {
        return concat(node2s(get(n, sym(cond))),
                      sym(?),
                      node2s(get(n, sym(then))),
                      sym(:),
                      node2s(get(n, sym(else))));
    }

    // really just a special kind of jump
    if (k ==  sym(return)) 
        return concat(stringify("return"), stringify(" "), node2s(get(n, sym(retval))));

    // this is if we need to introduce new bindings?
    //    if (kind ==  sym(compound_stmt)) {
    //        bprintf(b, "{");
    //        njoin(b, get(n, sym(stmts)), semicolon);                
    //        bprintf(b, "}");
    //        return;
    //    }

    // maybe?
    //    if (kind ==  sym(label_addr)) {
    //        bprintf(b, "&&%s", name);
    //        return;
    //    }
    halt("well, shit");
}

string string_from_token(tuple tok) {
    value k = get(tok, sym(kind));
    if  (k == sym(indent)) return get(tok, sym(value));
    if (k == sym(keyword)) return get(tok, sym(id));
    if (k == sym(char))  concat(stringify("'"), get(tok, sym(value)), stringify("'"));
    return get(k, sym(value));
}


value keys(value a)
{
    nursery n = allocate_nursery(nzv(a));
    foreach(k, v, a) {
        push_mut_value(n, k);
        value z = one;
        push_mut_value(n, z);
    }
    return table_from_nursery(n);
}

// to c
string emit_expression(value v)
{
    value vv;
    
    if ((vv = get(v, sym(value)))) return print(vv);
    
    return concat(stringify("("),
                  emit_expression(get(v, sym(left))),
                  get(v, sym(operation)),
                  emit_expression(get(v, sym(right))),
                  stringify(")"));
}
