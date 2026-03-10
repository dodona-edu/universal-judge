#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char input[1024];
    if (fgets(input, 1024, stdin)) {
        input[strcspn(input, "\n")] = 0;
    }

    FILE *f = fopen("input2.txt", "r");
    if (f) {
        char buffer[1024];
        if (fgets(buffer, 1024, f)) {
            buffer[strcspn(buffer, "\r\n")] = 0;
            if (strlen(buffer) > 0) {
                printf("%s\n", buffer);
                fclose(f);
                return 0;
            }
        }
        fclose(f);
    }

    printf("%s\n", input);

    return EXIT_SUCCESS;
}
