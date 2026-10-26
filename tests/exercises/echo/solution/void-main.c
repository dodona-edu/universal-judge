#include <stdio.h>

void main() {
    char line[100];
    if (fgets(line, sizeof line, stdin) != NULL) {
        printf("%s", line);
    }
}
