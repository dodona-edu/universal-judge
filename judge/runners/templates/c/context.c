## Code to execute_module one test context.
<%! from testplan import Assignment %>

#include <stdio.h>

#include <values.h>

void write_delimiter(FILE* value, FILE* exception, const char* delimiter) {
    fprintf(value, "%s", delimiter);
    fprintf(exception, "%s", delimiter);
}


int runner() {
    // Open the files for the values.
    FILE* value = fopen("${value_file}", "w");
    FILE* exception = fopen("${exception_file}", "w");

    // Main functions are not support at the moment.
    % if main_testcase.exists:
        write_delimiter(value, exception, "--${secret_id}-- SEP")
        printf("--${secret_id}-- SEP")
        fprintf(stderr, "--${secret_id}-- SEP")
    % endif

    ## Generate the actual tests based on the context.
    % for additional in additional_testcases:
        % if isinstance(additional.statement, Assignment):
            <%include file="assignment.mako" args="assignment=additional.statement" />\
        % else:
            % if additional.has_return:
                v_evaluate_${loop.index}(\
            % endif
            <%include file="function.mako" args="function=additional.statement" />\
            % if additional.has_return:
                );\
            % else:
                ;\
            % endif
        % endif

        write_delimiter(value, exception, "--${secret_id}-- SEP");
        printf("--${secret_id}-- SEP");
        fprintf(stderr, "--${secret_id}-- SEP");

    % endfor

    % if after:
        ${after}
    % endif

    fclose(value);
    fclose(exception);
}

#ifndef INCLUDED
int main() {
    runner();
}
#endif
