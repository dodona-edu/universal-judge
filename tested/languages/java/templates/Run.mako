## Code to execute_module one test context.
<%! from tested.languages.generator import _TestcaseArguments %>\
<%! from tested.serialisation import Statement, Expression, Assignment %>\
<%! from tested.utils import get_args %>\
import java.io.*;
import java.util.*;
import java.util.function.*;
import java.math.BigInteger;
import java.math.BigDecimal;

public class ${execution_name} implements Closeable {

    ##################################
    ## Setup                        ##
    ##################################

    ## Prepare the evaluator files.
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;

    public ${execution_name}() throws Exception {
        this.valueWriter = new PrintWriter("${value_file}");
        this.exceptionWriter = new PrintWriter("${exception_file}");
    }

    private void writeSeparator() throws Exception {
        valueWriter.write("--${secret_id}-- SEP");
        exceptionWriter.write("--${secret_id}-- SEP");
        System.err.print("--${secret_id}-- SEP");
        System.out.print("--${secret_id}-- SEP");
        valueWriter.flush();
        exceptionWriter.flush();
        System.err.flush();
        System.out.flush();
    }

    private void writeContextSeparator() throws Exception {
        valueWriter.write("--${context_secret_id}-- SEP");
        exceptionWriter.write("--${context_secret_id}-- SEP");
        System.err.print("--${context_secret_id}-- SEP");
        System.out.print("--${context_secret_id}-- SEP");
        valueWriter.flush();
        exceptionWriter.flush();
        System.err.flush();
        System.out.flush();
    }

    ##################################
    ## Predefined functions         ##
    ##################################

    ## Send a value to TESTed.
    private void sendValue(Object value) throws Exception {
        Values.send(valueWriter, value);
    }

    ## Send an exception to TESTed.
    private void sendException(Throwable exception) throws Exception {
        Values.sendException(exceptionWriter, exception);
    }

    ## Send the result of a language specific value evaluator to TESTed.
    private void sendSpecificValue(EvaluationResult value) {
        Values.sendEvaluated(valueWriter, value);
    }

    ## Send the result of a language specific exception evaluator to TESTed.
    private void sendSpecificException(EvaluationResult exception) {
        Values.sendEvaluated(exceptionWriter, exception);
    }

    % for i, ctx in enumerate(contexts):
        private void context${i}() throws Exception {
            ${ctx.before}

            % for tc in ctx.testcases:
                this.writeSeparator();

                ## In Java, we need special code to make variables available outside of
                ## the try-catch block.
                % if not tc.testcase.is_main_testcase() and isinstance(tc.input.command, get_args(Assignment)):
                    <%include file="declaration.mako"
                              args="tp=tc.input.command.type,value=tc.input.command.expression" /> \
                    ${tc.input.command.variable} = null;
                % endif

                try {
                    % if tc.testcase.is_main_testcase():
                        ${submission_name}.main(new String[]{\
                            % for argument in tc.input.arguments:
                                "${argument}", \
                            % endfor
                        });
                    % else:
                        <%include file="statement.mako" args="statement=tc.input.input_statement()" />;
                    % endif
                    <%include file="statement.mako" args="statement=tc.exception_statement()" />;
                } catch (Exception | AssertionError e) {
                    <%include file="statement.mako" args="statement=tc.exception_statement('e')" />;
                }
            % endfor

            ${ctx.after}
        }
    % endfor

    ## Most important function: actual execution happens here.
    void execute() throws Exception {
        % for i, ctx in enumerate(contexts):
            this.writeContextSeparator();
            this.context${i}();
        % endfor
    }

    @Override
    public void close() throws IOException {
        this.valueWriter.close();
        this.exceptionWriter.close();
    }

    public static void main(String[] a) throws Exception {
        try(${execution_name} execution = new ${execution_name}()) {
            execution.execute();
        }
    }
}
