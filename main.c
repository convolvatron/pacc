#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>
#include <string.h>

int main(int argc, char **argv)
{
    char *x="int main() {return 5;}";
    bits blen = (sizeof(x) -1) *8;
    buffer b = allocate_buffer(blen);
    memcpy(contents(b), x, blen>>3);
    parse_init(b);
}

