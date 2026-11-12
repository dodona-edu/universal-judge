#include <stdio.h>
#include <stdlib.h>

int total = 0;

int main() {

    int c;

    while ((c = getchar()) != EOF) {
        putchar(c);
        total++;
    }

    return EXIT_SUCCESS;
}
