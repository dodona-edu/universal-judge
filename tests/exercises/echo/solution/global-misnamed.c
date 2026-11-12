#include <stdio.h>
#include <stdlib.h>

int totl = 0;

int main() {

    int c;

    while ((c = getchar()) != EOF) {
        putchar(c);
        totl++;
    }

    return EXIT_SUCCESS;
}
