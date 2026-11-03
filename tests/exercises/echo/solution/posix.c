#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char *line = NULL;
    size_t size = 0;

    while (getline(&line, &size, stdin) != -1) {
        char *copy = strdup(line);
        fputs(copy, stdout);
        free(copy);
    }

    free(line);
    return 0;
}
