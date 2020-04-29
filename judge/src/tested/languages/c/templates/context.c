## Code to execute_module one context.
<%! from tested.languages.generator import _TestcaseArguments %>
<%! from tested.serialisation import Statement, Expression %>
<%! from tested.utils import get_args %>

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

## Write the delimiter and flush to ensure the output is in the files.
## This is necessary, otherwise the delimiters are sometimes missing when
## execution is killed due to timeouts.
static void ${context_name}_write_delimiter() {
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
#undef send
#define send(value) write_value(${context_name}_value_file, value)

## Send the result of a language specific value evaluator to TESTed.
#undef send_specific_value
#define send_specific_value(r) send_evaluated(${context_name}_value_file, r.result, r.readableExpected, r.readableActual, r.nrOfMessages, r.messages)

##################################
## Other testcase evaluators    ##
##################################
% for testcase in testcases:
    % if testcase.value_function:
        #define ${context_name}_v_evaluate_${loop.index}(value) <%include file="statement.mako" args="statement=testcase.value_function"/>
    % endif
% endfor


int ${context_name}() {

    ${context_name}_value_file = fopen("${value_file}", "w");
    ${context_name}_exception_file = fopen("${exception_file}", "w");

    ${before}

    ${context_name}_write_delimiter();

    // Main functions are not support at the moment.
    // TODO: arguments?
    % if context_testcase.exists:
        solution_main();
    % endif

    ## Generate the actual tests based on the context.
    % for testcase in testcases:
        ${context_name}_write_delimiter();
        ## If we have a value function, we have an expression.
        % if testcase.value_function:
            ${context_name}_v_evaluate_${loop.index}(\
        % endif
        <%include file="statement.mako" args="statement=testcase.command" />\
        % if testcase.value_function:
            )\
        % endif
        ;

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
