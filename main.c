#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>
#include <stdlib.h>

char *a[]={
           "struct z {int y; int f};",
           "int x = 2 + y * 7 + 3;",
           "int main() {return 5;}"};

int main(int argc, char **argv)
{
    runtime_init();
    for (int i = 0; i < sizeof(a)/sizeof(char *); i++) {
        int len = 0; 
        while (a[i][len]) len++;
        parse(allocate_utf8((u8 *)a[i], len));
    }
}

