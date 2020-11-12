#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>
#include <string.h>

value get(value, value);

void errorf(value v, char *fmt, ...)
{
}

void build2(lexer lex, ...)
{
    char *x;
    int total = 0; 
    foreach_arg(lex, i)
        printf("%p\n", i);
}

int main(int argc, char **argv)
{
    char *x="int main() {return 5;}";
    runtime_init();
    build2(0, "b", "c", INVALID_ADDRESS);
    bits blen = (sizeof(x) -1) *8;
    buffer b = allocate(tag_utf8, blen);
    memcpy(contents(b), x, blen>>3);
    lexer lex = create_lex(b);
    //    parse_init(b);
    value t;
    while ((t=get_token(lex)) && get(t, sym(kind)) != sym(eof)) {
        print(t);
    }
}

