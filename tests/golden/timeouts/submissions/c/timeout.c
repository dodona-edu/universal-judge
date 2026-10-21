#include <stdio.h>

void print_greeting(char* name) {
    volatile int forever = 1;
    while (forever) {}
    printf("Hello, %s!\n", name);
}
