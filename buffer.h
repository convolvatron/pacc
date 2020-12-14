#define contents(__x) ((void **)&(__x)->contents)
#define contents64(__x) ((u64 *)&(__x)->contents)
#define contentsu8(__x) ((u8 *)&(__x)->contents)
#define length(__x) ((void *)&(__x)->length)

typedef struct buffer {
    u64 length;
    u64 hash; // ?  
    u8 contents[];
} *buffer;

// ^ fort
#define scan_buffer(__i, __t, __stride, _ty)\
    for (void *__j = contents(__t), *__end = __j+(( __t)->length>>6); (__i = __j), (__j<__end); __j += __stride/8)


static inline buffer substring(buffer b, bits start, bits end)
{
    return allocate_utf8(contentsu8(b)+(start>>3), bytesof(end-start));
}


// need to get 1 bits set
// doesn't work for > 64 bits..umm, yes...up to 2^64?
// doesn't work for < 64 bits .. we were going to assume padding?... doesn't
// help for cstrings
static inline u64 hash_bitstring(u8 *x, u64 bits)
{
    u64 out = 0, k;

    for (int offset = 0; offset < bits ; offset += bitsizeof(u64)) {
        u64 k = ((u64 *)x)[offset>>6];
        int total = bits - offset;
        if (total < 64) k &= ((1<< total)-1); //endian?
        out ^= hash_imm(k, offset);
    }
    // multiple words match w/ out<<1? above?
    return out;
}


typedef u32 character; 

// maybe a star?
static inline bits utf8_length(u32 x)
{
    if (~x & 0x80) return 8;
    if ((x & 0xe0) == 0xc0) return 16;
    if ((x & 0xf0) == 0xe0) return 24;
    if ((x & 0xf8) == 0xf0) return 32;
    halt("invalid utf8 character");
}

// we know that a small is at least a u32 and a u32 can represent
// all utf8 codepoints
static inline u32 characterof(buffer b, bits offset)
{
    u32 x = 0;
    u8 *p = contentsu8(b) + bytesof(offset);
    int len = utf8_length(*p);
    __builtin_memcpy(&x, p, bytesof(len));
    return x;
}

