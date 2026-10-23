#include <stdio.h>

// int main() {
int main() {
    char line[100];
    if (fgets(line, sizeof line, stdin) != NULL) {
        printf("%s", line);
    }
}
