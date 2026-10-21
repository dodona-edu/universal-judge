#include <stdio.h>
#include <string.h>

int main(int argc, char** argv) {
    if (argc < 2) {
        return 1;
    }

    char* prefix = argv[1];
    for (int i = 2; i < argc; i++) {
        printf("%s: %s\n", prefix, argv[i]);
    }

    char line[256];
    while (fgets(line, sizeof line, stdin)) {
        line[strcspn(line, "\n")] = '\0';
        printf("%s\n", line);
    }
    return 0;
}
