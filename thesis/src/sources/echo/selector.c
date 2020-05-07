#include <string.h>
#include <stdio.h>
#define INCLUDED true
#include "context_0_0.c"
#include "context_0_1.c"
int main(int argc, const char* argv[]) {
    if (argc < 1) {
        fprintf(stderr, "You must select the context");
        return -2;
    }
    const char* name = argv[1];
    if (strcmp("context_0_0", name) == 0) {
        return context_0_0();
    }
    if (strcmp("context_0_1", name) == 0) {
        return context_0_1();
    }
    return -1;
}