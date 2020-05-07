#include <stdio.h>
#include "values.h"
#include "submission.c"
static FILE* context_0_0_value_file = NULL;
static FILE* context_0_0_exception_file = NULL;
static void context_0_0_write_separator() {
    fprintf(context_0_0_value_file, "--ovxWVU7E8-- SEP");
    fprintf(context_0_0_exception_file, "--ovxWVU7E8-- SEP");
    fprintf(stdout, "--ovxWVU7E8-- SEP");
    fprintf(stderr, "--ovxWVU7E8-- SEP");
}
#undef send_value
#define send_value(value) write_value(context_0_0_value_file, value)
#undef send_specific_value
#define send_specific_value(r) send_evaluated(context_0_0_value_file, r)
int context_0_0() {
    context_0_0_value_file = fopen("ovxWVU7E8_values.txt", "w");
    context_0_0_exception_file = fopen("ovxWVU7E8_exceptions.txt", "w");
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