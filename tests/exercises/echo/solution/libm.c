#include <math.h>
#include <stdio.h>

int main() {
    int c;

    while ((c = getchar()) != EOF) {
        putchar((int) sqrt((double) c * c));
    }

    return 0;
}
