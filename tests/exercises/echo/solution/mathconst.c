#include <math.h>
#include <stdio.h>

int main() {
    int c;

    while ((c = getchar()) != EOF) {
        putchar((int) (c + M_PI - M_PI + 0.5));
    }

    return 0;
}
