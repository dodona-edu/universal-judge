#include <ctype.h>

void trim(char *s) {
    char * p = s;
    int l = strlen(p);

    while(isspace(p[l - 1])) p[--l] = 0;
    while(*p && isspace(* p)) ++p, --l;

    memmove(s, p, l + 1);
}

char* echo_file(const char* filename) {
    char *buffer = 0;
    long length;
    FILE * f = fopen(filename, "rb");

    if (f) {
      fseek (f, 0, SEEK_END);
      length = ftell (f);
      fseek (f, 0, SEEK_SET);
      buffer = malloc (length);
      if (buffer) {
        fread (buffer, 1, length, f);
      }
      fclose (f);
    }
    if (buffer) {
        trim(buffer);
        return buffer;
    }
    return "";
}
