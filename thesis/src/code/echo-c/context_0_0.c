#include <stdio.h>

#include "values.h"
#include "submission.c"

static FILE* context_0_0_value_file = NULL;
static FILE* context_0_0_exception_file = NULL;

static void context_0_0_write_separator() {
    fprintf(context_0_0_value_file, "--iIyiUwfg3-- SEP");
    fprintf(context_0_0_exception_file, "--iIyiUwfg3-- SEP");
    fprintf(stdout, "--iIyiUwfg3-- SEP");
    fprintf(stderr, "--iIyiUwfg3-- SEP");
}

#undef send_value
#define send_value(value) write_value(context_0_0_value_file, value)

#undef send_specific_value
#define send_specific_value(r) send_evaluated(context_0_0_value_file, r)

int context_0_0() {

    context_0_0_value_file = fopen("iIyiUwfg3_values.txt", "w");
    context_0_0_exception_file = fopen("iIyiUwfg3_exceptions.txt", "w");

    context_0_0_write_separator();

    solution_main();
    
    fclose(context_0_0_value_file);
    fclose(context_0_0_exception_file);
    return 0;
}

#ifndef INCLUDED
int main() {
    return context_0_0();
}
#endif