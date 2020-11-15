#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>
#include <string.h>

value get(value, value);

void errorf(value v, char *fmt, ...)
{
}

int main(int argc, char **argv)
{
    char *x="int main() {return 5;}";
    runtime_init();
    bits blen = (sizeof(x) -1) *8;
    buffer b = allocate(tag_utf8, blen);
    memcpy(contents(b), x, blen>>3);
    lexer lex = create_lex(b);
    //    parse_init(b);
    value t;
    while ((t=get_token(lex)) && get(t, stringify("kind")) != stringify("eof")) {
        print(t);
    }
}

