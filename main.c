#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>

buffer read_file(string filename)
{
    struct stat st;
    unsigned char *n = aprintf("%s\0", filename)->contents;
    if (stat((const char*)n, &st) < 0) {
        halt("no such file %b", filename);
    }
    int len = st.st_size;
    int fd = open((const char*)n, O_RDONLY);
    if (fd < 0) {
        error("couldn't open file %b", filename);
    }
    buffer out = allocate_buffer(len);
    int r = read(fd, out->contents, len);
    if (r != len) {
        error("short read %b", filename);
    }
    out->end = len;
    return out;
}

int main(int argc, char **argv)
{
    for (int i = 1 ;i <argc; i++ ){
        standardout(aprintf ("%v", parse_init(read_file(cstring(argv[i])))));
    }
}

