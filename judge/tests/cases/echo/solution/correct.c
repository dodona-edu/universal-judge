#include <stdio.h>
#include <stdlib.h>

int main() {

    int c;

    while ((c = getchar()) != EOF) {
        putchar(c);
    }

    return EXIT_SUCCESS;
}
