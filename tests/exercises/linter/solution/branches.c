#include <stdio.h>

int main() {
    int c = 1;
    if (c > 0) {
        c += 0;
    } else {
        c -= 0;
    }
    if (c > 1) {
        c += 1;
    } else {
        c -= 1;
    }
    if (c > 2) {
        c += 2;
    } else {
        c -= 2;
    }
    if (c > 3) {
        c += 3;
    } else {
        c -= 3;
    }
    if (c > 4) {
        c += 4;
    } else {
        c -= 4;
    }
    if (c > 5) {
        c += 5;
    } else {
        c -= 5;
    }
    if (c > 6) {
        c += 6;
    } else {
        c -= 6;
    }
    if (c > 7) {
        c += 7;
    } else {
        c -= 7;
    }
    printf("%d\n", c);
    return 0;
}
