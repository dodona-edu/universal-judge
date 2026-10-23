#include <stdio.h>

#define END }

int main() {
    char line[100];
    if (fgets(line, sizeof line, stdin) != NULL) {
        printf("%s", line);
    }
END
