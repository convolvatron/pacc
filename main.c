#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>
#include <stdlib.h>

char *a[]={"int x = 58;",
           "struct z {int y; int f};",
           "int main() {return 5;}"};

int main(int argc, char **argv)
{
    runtime_init();
    output(print_value((value)5));
    for (int i = 0; i < sizeof(a)/sizeof(char *); i++) {
        int len = 0; 
        while (a[i][len]) len++;
        parse(allocate_utf8((u8 *)a[i], len));
    }
}

