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
    value whitespace;
    if (!whitespace)
        whitespace = set((value)' ',(value)'\t',(value)'\n');
    
    offset = forc(b, offset, get(whitespace, (value)c));
    if (characterof(b, offset) == '(') {
        nursery n = allocate_nursery(10);
        while (characterof(b, offset) != ')') {
            result k = parse_value(b, offset+1);
            push_mut_value(n, k.v);
            if (characterof(b, offset) != ':') halt("invalid separator");
            result v = parse_value(b, k.offset);
            push_mut_value(n, v.v);            
            offset = v.offset;
        }
        return res(table_from_nursery(n), offset+1);
    } else {
        u64 end = forc(b, offset, (c != ':') && (c !='(') && (!get(whitespace, (value)c)));
        // shouldn't be ripping trailing whitespace? - just to clean up the above
        return res(substring (b, offset, end),
                   forc(b, offset, get(whitespace, (value)c)));
    }
}

extern u8 *_binary_syntax_start;
u64 _binary_syntax_size;

value world()
{
    // doesn't work on mac
    result r = parse_value(allocate_utf8(_binary_syntax_start, bitsof(_binary_syntax_size)), 0);
    return r.v;
}
