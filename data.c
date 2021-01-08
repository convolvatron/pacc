// just using the result defintion because its handy - this isn't part of the parser
#include <pacc.h>

// fall off end
#define forc(__b, __offset, __cond) ({\
  u64 __o2 = __offset;\
  for (character c; c = characterof(__b, __o2), (__o2 < __b->length) && (__cond); __o2 += utf8_length(c)); \
  __o2;\
})

result parse_value(buffer b, u64 offset)
{
    static value whitespace = 0, terminus;
    if (!whitespace) {
        whitespace = set((value)' ',(value)'\t',(value)'\n');
        value special = set((value)'(',(value)')',(value)':');
        terminus = combine(whitespace, special);
    }

    offset = forc(b, offset, get(whitespace, (value)c));    
    switch (characterof(b, offset)) {
    case '(': {
        nursery n = allocate_nursery(10);
        while (characterof(b, offset) != ')') {
            result k = parse_value(b, offset+8);
            push_mut_value(n, k.v);
            offset = forc(b, k.offset, get(whitespace, (value)c));            
            if (characterof(b, offset) != ':') halt("invalid separator");
            result v = parse_value(b, offset+8);
            push_mut_value(n, v.v);            
            offset = v.offset;
        }
        return res(table_from_nursery(n), offset+8);
    }
    // this has lots of issues....but for now...
    case '#':
        {
            u64 end = forc(b, offset, !get(terminus, (value)c));
            // translate as hex
            return res(substring (b, offset, end), end);
        }
    default:
        {
            u64 end = forc(b, offset, !get(terminus, (value)c));
            return res(substring (b, offset, end), end);
        }
    }
}

extern u8 _binary_syntax_start;
extern u8 _binary_syntax_end;

value world()
{
    // doesn't work on mac
    result r = parse_value(allocate_utf8(&_binary_syntax_start,
                                         &_binary_syntax_end  - &_binary_syntax_start),
                           0);
    return r.v;
}
