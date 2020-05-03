## Code to execute_module one context.

#include <stdio.h>

#include "values.h"
#include "${submission_name}.c"

##################################
## Setup                        ##
##################################

## Import the language specific evaluators we will need.
% for name in evaluator_names:
    #include "${name}.c"
% endfor

static FILE* ${context_name}_value_file = NULL;
static FILE* ${context_name}_exception_file = NULL;

## Write the separator and flush to ensure the output is in the files.
## This is necessary, otherwise the separators are sometimes missing when
## execution is killed due to timeouts.
static void ${context_name}_write_separator() {
    fprintf(${context_name}_value_file, "--${secret_id}-- SEP");
    fprintf(${context_name}_exception_file, "--${secret_id}-- SEP");
    fprintf(stdout, "--${secret_id}-- SEP");
    fprintf(stderr, "--${secret_id}-- SEP");
}

##################################
## Predefined functions         ##
##################################

## Send a value to TESTed.
## Use a macro, since there are no generics in C.
#undef send_value
#define send_value(value) write_value(${context_name}_value_file, value)

## Send the result of a language specific value evaluator to TESTed.
#undef send_specific_value
#define send_specific_value(r) send_evaluated(${context_name}_value_file, r)


int ${context_name}() {

    ${context_name}_value_file = fopen("${value_file}", "w");
    ${context_name}_exception_file = fopen("${exception_file}", "w");

    ${before}

    ${context_name}_write_separator();

    // Main functions are not support at the moment.
    // TODO: arguments?
    % if context_testcase.exists:
        solution_main();
    % endif

    ## Generate the actual tests based on the context.
    % for testcase in testcases:
        ${context_name}_write_separator();
        <%include file="statement.mako" args="statement=testcase.input_statement()" />;
        
    % endfor

    ${after}

    fclose(${context_name}_value_file);
    fclose(${context_name}_exception_file);
    return 0;
}

#ifndef INCLUDED
int main() {
    return ${context_name}();
}
#endif
