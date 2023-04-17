#include <stdio.h>;

int main() {
    fprintf(stderr, "Ooeps");
    int i[1] = {0};
    i[8000] = 30;
    return i[0];
}
