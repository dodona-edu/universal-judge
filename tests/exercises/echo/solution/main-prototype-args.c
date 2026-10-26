#include <stdio.h>

int main(int argc, char** argv);

int main(int argc, char** argv) {
    char line[100];
    if (fgets(line, sizeof line, stdin) != NULL) {
        printf("%s", line);
    }
}
