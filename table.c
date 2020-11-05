#include <runtime.h>

// hash of zero is zero - i think this has to be that way because of default-zero
// doesn't that mean that hash should be (n & v) or some such? except
// less bit-losing
u64 hash(value b)
{
    u64 h = 0;
    if (b == zero) return 0;
    if (b == one) return 1;
    // (0:1) => 1 structurally - so just n^v?
    // tag n value
    for (int i =b->start; i< b->end; i++ ) {
        // maka tag
    }
}

// can we collapse the representation of buffer and table, at least the
// size. a table is an open hash of nv pairs. and a number is a buffer.
// an empty cell is denoted by zero n, v. v = zero should never be
// stored since a table has implicit (default) mappings for all names to zero.

// ok - so how do i know if a certain bitstring is a map or a .. bitstring
// clearly from information in the address. so we have utf8s, numbers, and tables...
// and functions. of course (apparently) those need certain methods (get, iterate)
// in order to map their representations to the common object semantics. 


static void insert(buffer b, buffer k, buffer value)
{
}
    
void scopy_tuple(tuple parent, ...)
{
    int size = next_power_2(argcount(parent));
    buffer b = allocate_zero(size * bitsizeof(word) + bitsizeof(word));
    int k = 0;
    
    for_args(x, parent) {
        value last = 2;
        if (k++ == 1){
            insert(b, 
            buffer_read(b, 0);            
            buffer_write(b, 0);
        }
    }
}

// what is the union of scope? - it has to be the union of scopes. so scopes is a set?
// we can also flatten i guess?
void union(value nothing, ...)
{
    
}
