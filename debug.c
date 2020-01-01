// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "pacc.h"

#define sstring staticbuffer
// just keep this in the type
static string string_from_type(Type ty) {

    symbol kind = get(ty, sym(kind));
    if (kind == sym(ptr))
        return aprintf("*%s", string_from_type(get(ty, sym(pointsto))));
        
    if (kind == sym(array))                 
        return aprintf("[%d]%s", get(ty, sym(length)), string_from_type(get(ty, sym(pointsto))));
    
    if ((kind == sym(union)) || (kind == sym(struct)))  {
        //        symbol key = intern(aprintf(transient, "%p", ty));
        if (get(ty, sym(fields))) {
            buffer b = aprintf("(%s", kind);
            // ordering of struct entries .. matters semantically
            foreach(fkey, ftype, get(ty, sym(fields))) 
                bprintf(b, " (%s)", string_from_type(ftype));
            bprintf(b, ")");
            return b;
        }
    }
    
    if (kind == sym(func)) {
        buffer b = aprintf("(");
        if (get(ty, sym(parameters))) {
            boolean first = true;
            Type t;             
            foreach(_, t, get(ty, sym(params))) {
                if (!first)  bprintf(b, ",");
                first = false;
                bprintf(b, "%s", string_from_type(t));
            }
        }
        bprintf(b, ")=>%s", string_from_type(get(ty, sym(return_type))));
        return b;
    }
    return kind;
}

static void node2s(buffer b, Node node);

static void uop_to_string(buffer b, symbol op, Node node)
{
    bprintf(b, "(%s ", op);
    node2s(b, get(node, sym(operand)));
    bprintf(b, ")");
}

static void binop_to_string(buffer b, symbol s, Node node)
{
    bprintf(b, "(%b ", s);
    node2s(b, get(node, sym(left)));
    bprintf(b, " ");
    node2s(b, get(node, sym(right)));
    bprintf(b, ")");    
}

static void njoin(buffer dest, vector list, char sep)
{
    boolean first = true;
    value i;
    foreach(_, i, list) {
        if (!first) buffer_write_byte(dest, sep);
        first = false;
        node2s(dest, i);
    }
}

static void a2s_declinit(buffer b, vector initlist) {
    njoin(b, initlist, ' ');
}

int value_to_integer(value v)
{
    return 0;
}
  
static void node2s(buffer b, Node n) {
    if (!n) {
        bprintf(b, "(nil)");
        return;
    }
    symbol kind = get(n, sym(kind));
    symbol name = get(n, sym(name));
    // should be the object, not the name - fix
    if (get(n, sym(binops), kind)) {
        binop_to_string(b, kind, n);
    } else {
        if (get(n, sym(uops), kind)){        
            uop_to_string(b, kind, n);                
        } else {
            if (kind == sym(literal)) {
                symbol ntk = get(n, sym(type), sym(kind));
                if (ntk == sym(char)) {
                    int i = value_to_integer(get(n, sym(integer_value)));
                    if (i == '\n')      bprintf(b, "'\n'");
                    else if (i == '\\') bprintf(b, "'\\\\'");
                    else if (i == '\0') bprintf(b, "'\\0'");
                    else bprintf(b, "'%c'", i);
                    return;
                }
                if ((ntk == sym(int)) ||
                    (ntk == sym(long)) ||
                    (ntk == sym(llong))) {
                    int i = value_to_integer(get(n, sym(integer_value)));
                    bprintf(b, "%d", i);
                    return;
                }
                if (ntk == sym(array)) {
                    bprintf(b, "\"%b\"", get(n, sym(sval)));
                }
            }
            if (kind == sym(label)) {
                bprintf(b, "%s:", name);
                return;
            }
            if (kind ==  sym(variable)){
                bprintf(b, "lv=%s", name);
                value init = get(n, sym(lvarinit));
                if (init) {
                    bprintf(b, "(");
                    a2s_declinit(b, init);
                    bprintf(b, ")");
                }
            }

            value nty = get(n, sym(type));
            // xx why are these really so different, isn't this just
            // an expression?
            if ((kind == sym(funcall)) || (kind == sym(funcptr_call))) {
                bprintf(b, "(%s)", string_from_type(nty));
                if (kind == sym(funcall))
                    push_buffer(b, name);
                else
                    node2s(b, n);
                njoin(b, get(n, sym(arguments)), ',');
                bprintf(b, ")");
                return;
            }
            
            if (kind == sym(funcdesg)) {
                bprintf(b, "(funcdesg %s)", name);
                return;
            }
            
            if (kind == sym(func)) {
                bprintf(b, "(%s)%s(", string_from_type(nty), name);
                njoin(b, get(nty, sym(params)), ',');
                bprintf(b, ")");
                node2s(b, get(n, sym(body)));
                return;
            }
            
            if (kind ==  sym(goto)) {
                bprintf(b, "goto(%s)", name);
                return;
            }
            
            if (kind ==  sym(decl)){
                bprintf(b, "(decl %b %b",
                        string_from_type(get(n, sym(declvar), sym(type))),
                        get(n, sym(declvar), sym(name)));
                if ( get(n, sym(declinit))){
                    bprintf(b, " ");
                    a2s_declinit(b, get(n, sym(init)));
                }
                bprintf(b, ")");
                return;
            }
            
            if (kind ==  sym(init)) {
                node2s(b, get(n, sym(initval)));
                return;
            }

            if (kind ==  sym(conv)) {
                bprintf(b, "(conv ");
                node2s(b, get(n, sym(operand)));
                bprintf(b, " =>%s)", string_from_type(nty));
                return;
            }
            
            if (kind ==  sym(if)) {
                bprintf(b, "(if ");
                node2s(b, get(n, sym(cond)));
                bprintf(b, " ");            
                node2s(b, get(n, sym(then)));
                value e = get(n, sym(else));
                if (e) {
                    bprintf(b, " ");                 
                    node2s(b, e);
                }
                bprintf(b, ")");
                return;
            }
            
            if (kind ==  sym(ternary)) {
                bprintf(b, "(? ");
                node2s(b, get(n, sym(cond)));                
                bprintf(b, " ");
                node2s(b, get(n, sym(then)));                
                bprintf(b, " ");
                node2s(b, get(n, sym(els)));                
                return;
            }

            if (kind ==  sym(return)) {
                bprintf(b, "(return ");
                // so many kinds of arguments
                node2s(b, get(n, sym(retval)));                                
                bprintf(b, ")");
                return;
            }
            
            if (kind ==  sym(compound_stmt)) {
                bprintf(b, "{");
                njoin(b, get(n, sym(stmts)), ';');                
                bprintf(b, "}");
                return;
            }
            
            if (kind ==  sym(struct_ref)){
                node2s(b, get(n, sym(struc)));
                bprintf(b, ".");
                push_buffer(b, get(n, sym(field)));
                return;
            }
            
            if (kind ==  sym(cast)) {
                bprintf(b, "((%b)=>(%b) ",
                        string_from_type(get(n, sym(operand), sym(type))),
                        string_from_type(nty));
                node2s(b, get(n, sym(operand)));
                bprintf(b, ")");
                return;
            }
            
            if (kind ==  sym(label_addr)) {
                bprintf(b, "&&%s", name);
                return;
            }
            
            if (kind == sym(=))
                bprintf(b, "(== ");
            else
                bprintf(b, "(%c ", kind);
            node2s(b, get(n, sym(left)));
            bprintf(b, " ");
            node2s(b, get(n, sym(right)));
            bprintf(b, ")");        
        }
    }
}

string string_from_token(heap h, tuple tok) {
    value k = get(tok, sym(kind));
    if  (k == sym(indent)) return get(tok, sym(value));
    if (k == sym(keyword)) return get(tok, sym(id));
    if (k == sym(char))  aprintf("'%s'", sym(value));
    return get(k, value);
}
