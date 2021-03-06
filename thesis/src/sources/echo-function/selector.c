#include <string.h>
#include <stdio.h>
#define INCLUDED true
#include "context_0_0.c"
int main(int argc, const char* argv[]) {
    if (argc < 1) {
        fprintf(stderr, "No context selected.");
        return -2;
    }
    const char* name = argv[1];
    if (strcmp("context_0_0", name) == 0) {
        return context_0_0();
    }
    fprintf(stderr, "Non-existing selector '%s' selected.", name);
    return -1;
}