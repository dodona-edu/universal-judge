## Code to execute_module one test context.
<%! from testplan import Assignment %>
<%! import humps %>
## This imports are defined by the "common" start-up scripts of JShell.
import java.io.*;
import java.math.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;
import java.util.prefs.*;
import java.util.regex.*;
import java.util.stream.*;

public class ${context_name} {

    ## Generate the evaluators we need.
    % for name in evaluator_names:
        <% var_name = humps.camelize(name) %>
        private final ${name} ${var_name} = new ${name}();
    % endfor

    ## Prepare the evaluator files.
    private final FileWriter valueWriter;
    private final FileWriter exceptionWriter;

    public ${context_name}() throws Exception {
        this.valueWriter = new FileWriter("${value_file}");
        this.exceptionWriter = new FileWriter("${exception_file}");
    }

    private void writeDelimiter(String value) throws Exception {
        valueWriter.write(value);
        exceptionWriter.write(value);
    }

    private void evaluated(boolean result, String expected, String actual, Collection<String> messages) throws Exception {
        Values.evaluated(valueWriter, result, expected, actual, messages);
    }

    private void evaluated(boolean result, String expected, String actual) throws Exception {
        Values.evaluated(valueWriter, result, expected, actual, Collections.emptyList());
    }

    private void send(Object value) throws Exception {
        Values.send(valueWriter, value);
    }

    private void sendException(Exception exception) throws Exception {
        Values.sendException(exceptionWriter, exception);
    }

    private void eEvaluateMain(Exception value) throws Exception {
        <%include file="function.mako" args="function=main_testcase.exception_function"/>;
    }

    % for additional in additional_testcases:
        % if additional.has_return:
            private void vEvaluate${loop.index}(Object value) throws Exception {
                <%include file="function.mako" args="function=additional.value_function"/>;
            }
        % endif

        private void eEvaluate${loop.index}(Exception value) throws Exception {
            <%include file="function.mako" args="function=additional.exception_function"/>;
        }
    % endfor

    void execute() throws Exception {
        ## In Java, we must execute_module the before and after code in the context.
        ${before}

        ## Call the context_testcase fucnction if necessary
        % if main_testcase.exists:
            try {
                ${submission_name}.main(new String[]{
                % for argument in main_testcase.arguments:
                    <%include file="literal.mako" args="value=argument"/>
                    % if not loop.last:
                        , \
                    % endif
                % endfor
                });
            } catch (Exception e) {
                this.eEvaluateMain(e);
            }
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            this.writeDelimiter("--${secret_id}-- SEP");
        % endif

        % for additional in additional_testcases:
            % if isinstance(additional.statement, Assignment):
                <%include file="declaration.mako" args="value=additional.statement.get_type()" /> ${additional.statement.name} = null;
            % endif
            try {
                % if isinstance(additional.statement, Assignment):
                    <%include file="assignment.mako" args="assignment=additional.statement" />
                % else:
                    % if additional.has_return:
                        this.vEvaluate${loop.index}(\
                    % endif
                    <%include file="function.mako" args="function=additional.statement" />\
                    % if additional.has_return:
                        )\
                    % endif
                    ;
                % endif
            } catch (Exception e) {
                this.eEvaluate${loop.index}(e);
            }
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            this.writeDelimiter("--${secret_id}-- SEP");
        % endfor

        ${after}
    }

    void close() throws Exception {
        this.valueWriter.close();
        this.exceptionWriter.close();
    }

    public static void main(String[] a) throws Exception {
        var context = new ${context_name}();
        context.execute();
        context.close();
    }
}
