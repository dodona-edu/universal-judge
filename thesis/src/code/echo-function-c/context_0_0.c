#include <stdio.h>

#include "values.h"
#include "submission.c"

static FILE* context_0_0_value_file = NULL;
static FILE* context_0_0_exception_file = NULL;

static void context_0_0_write_separator() {
    fprintf(context_0_0_value_file, "--SXjMtnKDI-- SEP");
    fprintf(context_0_0_exception_file, "--SXjMtnKDI-- SEP");
    fprintf(stdout, "--SXjMtnKDI-- SEP");
    fprintf(stderr, "--SXjMtnKDI-- SEP");
}

#undef send_value
#define send_value(value) write_value(context_0_0_value_file, value)

#undef send_specific_value
#define send_specific_value(r) send_evaluated(context_0_0_value_file, r)

#define context_0_0_v_evaluate_0(value) send_value(value)
#define context_0_0_v_evaluate_1(value) send_value(value)

int context_0_0() {

    context_0_0_value_file = fopen("SXjMtnKDI_values.txt", "w");
    context_0_0_exception_file = fopen("SXjMtnKDI_exceptions.txt", "w");

    context_0_0_write_separator();

    context_0_0_write_separator();
    context_0_0_v_evaluate_0(echo("input-1"));

    context_0_0_write_separator();
    context_0_0_v_evaluate_1(echo("input-2"));

    fclose(context_0_0_value_file);
    fclose(context_0_0_exception_file);
    return 0;
}

#ifndef INCLUDED
int main() {
    return context_0_0();
}
#endif