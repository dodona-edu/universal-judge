#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int add(int a, int b) {
    return a + b;
}

char* greet(char* name) {
    char* greeting = malloc(strlen(name) + 9);
    sprintf(greeting, "Hello, %s!", name);
    return greeting;
}

void print_greeting(char* name) {
    printf("Hello, %s!\n", name);
}
