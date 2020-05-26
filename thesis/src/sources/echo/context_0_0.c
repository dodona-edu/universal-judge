#include <stdio.h>
#include "values.h"
#include "submission.c"
static FILE* context_0_0_value_file = NULL;
static FILE* context_0_0_exception_file = NULL;
static void context_0_0_write_separator() {
    fprintf(context_0_0_value_file, "--bKHsgGplf-- SEP");
    fprintf(context_0_0_exception_file, "--bKHsgGplf-- SEP");
    fprintf(stdout, "--bKHsgGplf-- SEP");
    fprintf(stderr, "--bKHsgGplf-- SEP");
}
#undef send_value
#define send_value(value) write_value(context_0_0_value_file, value)
#undef send_specific_value
#define send_specific_value(value) write_evaluated(context_0_0_value_file, value)
int context_0_0() {
    context_0_0_value_file = fopen("bKHsgGplf_values.txt", "w");
    context_0_0_exception_file = fopen("bKHsgGplf_exceptions.txt", "w");
    context_0_0_write_separator();
    char* args[] = {    "solution",     };
    solution_main(1, args);
    fclose(context_0_0_value_file);
    fclose(context_0_0_exception_file);
    return 0;
}
#ifndef INCLUDED
int main() {
    return context_0_0();
}
#endif