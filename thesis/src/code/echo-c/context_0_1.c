#include <stdio.h>

#include "values.h"
#include "submission.c"

static FILE* context_0_1_value_file = NULL;
static FILE* context_0_1_exception_file = NULL;

static void context_0_1_write_separator() {
    fprintf(context_0_1_value_file, "--jLUzXq6kg-- SEP");
    fprintf(context_0_1_exception_file, "--jLUzXq6kg-- SEP");
    fprintf(stdout, "--jLUzXq6kg-- SEP");
    fprintf(stderr, "--jLUzXq6kg-- SEP");
}

#undef send_value
#define send_value(value) write_value(context_0_1_value_file, value)

#undef send_specific_value
#define send_specific_value(r) send_evaluated(context_0_1_value_file, r)

int context_0_1() {

    context_0_1_value_file = fopen("jLUzXq6kg_values.txt", "w");
    context_0_1_exception_file = fopen("jLUzXq6kg_exceptions.txt", "w");    

    context_0_1_write_separator();

    solution_main();

    fclose(context_0_1_value_file);
    fclose(context_0_1_exception_file);
    return 0;
}

#ifndef INCLUDED
int main() {
    return context_0_1();
}
#endif