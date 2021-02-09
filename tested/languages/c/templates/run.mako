## Code to execute_module one context.
#include <stdio.h>

#include "values.h"
#include "${submission_name}.c"

## Import the language specific evaluators we will need.
% for name in evaluator_names:
    #include "${name}.c"
% endfor

static FILE* ${execution_name}_value_file = NULL;
static FILE* ${execution_name}_exception_file = NULL;

static void ${execution_name}_write_separator() {
    fprintf(${execution_name}_value_file, "--${secret_id}-- SEP");
    fprintf(${execution_name}_exception_file, "--${secret_id}-- SEP");
    fprintf(stdout, "--${secret_id}-- SEP");
    fprintf(stderr, "--${secret_id}-- SEP");
}

static void ${execution_name}_write_context_separator() {
    fprintf(${execution_name}_value_file, "--${context_secret_id}-- SEP");
    fprintf(${execution_name}_exception_file, "--${context_secret_id}-- SEP");
    fprintf(stdout, "--${context_secret_id}-- SEP");
    fprintf(stderr, "--${context_secret_id}-- SEP");
}

## Send a value to TESTed.
## Use a macro, since there are no generics in C.
#undef send_value
#define send_value(value) write_value(${execution_name}_value_file, value)

## Send the result of a language specific value evaluator to TESTed.
#undef send_specific_value
#define send_specific_value(value) write_evaluated(${execution_name}_value_file, value)



% for i, ctx in enumerate(contexts):
    void ${execution_name}_context_${i}(void) {
        ${ctx.before}
        % for testcase in ctx.testcases:
            ${execution_name}_write_separator();
            <%include file="statement.mako" args="statement=testcase.input_statement()" />;
        % endfor
        ${ctx.after}
    }
% endfor

int ${execution_name}() {

    int ${execution_name}_exit_code = 0;

    ${execution_name}_value_file = fopen("${value_file}", "w");
    ${execution_name}_exception_file = fopen("${exception_file}", "w");

    ${execution_name}_write_context_separator();
    % if run_testcase.exists:
        char* args[] = {\
        % for argument in ["solution"] + run_testcase.arguments:
            "${argument}", \
        % endfor
        };
        ${execution_name}_exit_code = solution_main(${len(run_testcase.arguments) + 1}, args);
    % endif

    % for i, ctx in enumerate(contexts):
        ${execution_name}_write_context_separator();
        ${execution_name}_context_${i}();
    % endfor

    fclose(${execution_name}_value_file);
    fclose(${execution_name}_exception_file);
    return ${execution_name}_exit_code;
}

#ifndef INCLUDED
int main() {
    return ${execution_name}();
}
#endif
