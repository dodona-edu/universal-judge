## Code to execute_module one test context.
<%! from tested.serialisation import Statement, Expression %>
<%! from tested.utils import get_args %>

#include <stdio.h>

#include "values.h"
#include "${submission_name}.c"

//// Main function of the solution.
//int solution_main();

// TODO: evaluate return value somehow.

int ${context_name}() {
    // Open the files for the values.
    FILE* value = fopen("${value_file}", "w");
    FILE* exception = fopen("${exception_file}", "w");

    // Main functions are not support at the moment.
    % if context_testcase.exists:
        solution_main();
        // TODO: options
    % endif
    fprintf(value, "--${secret_id}-- SEP");
    fprintf(exception, "--${secret_id}-- SEP");
    fprintf(stdout, "--${secret_id}-- SEP");
    fprintf(stderr, "--${secret_id}-- SEP");

    ## Generate the actual tests based on the context.
    % for additional in testcases:
        % if isinstance(additional.command, get_args(Statement)):
            <%include file="statement.mako" args="statement=additional.command,full=True" />\
        % else:
            <% assert isinstance(additional.command, get_args(Expression)) %>
            % if additional.has_return:
                // TODO
            % endif
            <%include file="expression.mako" args="expression=additional.command" />;
            % if additional.has_return:
                // TODO
            % endif
        % endif

        fprintf(value, "--${secret_id}-- SEP");
        fprintf(exception, "--${secret_id}-- SEP");
        fprintf(stdout, "--${secret_id}-- SEP");
        fprintf(stderr, "--${secret_id}-- SEP");

    % endfor

    ${after}

    fclose(value);
    fclose(exception);
    return 0;
}

#ifndef INCLUDED
int main() {
    return ${context_name}();
}
#endif
