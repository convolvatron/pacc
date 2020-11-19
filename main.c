#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>
#include <string.h>
#include <stdlib.h>

value get(value, value);

void errorf(value v, char *fmt, ...)
{
}

int main(int argc, char **argv)
{
    char x[]="int main() {return 5;}";
    runtime_init();

    lexer lex = create_lex(allocate_utf8((u8 *)x, sizeof(x)-1));
    parse_init(lex);
}

