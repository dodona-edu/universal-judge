#include <stdio.h>

void echo_function(const char *filename, const char *string) {
    FILE *file = fopen(filename, "w");
    fprintf(file, "%s\n", string);
    fclose(file);
}
