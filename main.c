#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>
#include <string.h>
#include <stdlib.h>

value get(value, value);

char *a[]={"int x;",
           "struct z {int y; int f};",
           "int main() {return 5;}"};

int main(int argc, char **argv)
{
    runtime_init();

    for (int i = 0; i < sizeof(a)/sizeof(char *); i++) {
        lexer lex = create_lex(allocate_utf8((u8 *)a[i], sizeof(a[i])-1));
        parse(lex);
    }
}

