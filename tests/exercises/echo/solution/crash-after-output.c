#include <stdio.h>

int main() {
    int c;
    while ((c = getchar()) != EOF) {
        putchar(c);
    }
    fflush(stdout);
    *(volatile int *) NULL = 1;
    return 0;
}
