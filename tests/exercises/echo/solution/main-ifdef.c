#include <stdio.h>

#ifdef X
int main() {
#else
int main(void) {
#endif
    char line[100];
    if (fgets(line, sizeof line, stdin) != NULL) {
        printf("%s", line);
    }
}
