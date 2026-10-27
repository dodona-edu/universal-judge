#include <stdio.h>

int main(const int argc, char** argv) {
    if (argc < 1) {
        return 1;
    }
    char line[100];
    if (fgets(line, sizeof line, stdin) != NULL) {
        printf("%s", line);
    }
}
