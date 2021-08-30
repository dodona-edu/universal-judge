#include <stdio.h>

int main(int argc, char** argv) {
    char *getal = NULL;
    long unsigned int size;
    // Getal inlezen
    if (getline(&getal, &size, stdin) == -1) {
        printf("No input\n");
        return 1;
    }

    int cirkels = 0;
    // Aantal cirkels in de cijfers van het getal tellen
    for (int i = 0; getal[i] != '\0'; i++) {
        if (getal[i] == '8') {
            cirkels += 2;
        } else if (getal[i] == '0' || getal[i] == '4' || getal[i] == '6' || getal[i] == '9') {
            cirkels += 1;
        }
    }

    // Aantal cirkels uitschrijven
    printf("%d\n", cirkels);
    return 0;
}