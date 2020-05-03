## Code to execute_module one test context.
<%! from tested.languages.generator import _TestcaseArguments %>
<%! from tested.serialisation import Statement, Expression, Assignment %>
<%! from tested.utils import get_args %>
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

public class ${context_name} implements Closeable {

    ##################################
    ## Setup                        ##
    ##################################

    ## Prepare the evaluator files.
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;

    public ${context_name}() throws Exception {
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
    private void sendSpecificValue(EvaluationResult r) {
        Values.sendEvaluated(valueWriter, r);
    }

    ## Send the result of a language specific exception evaluator to TESTed.
    private void sendSpecificException(EvaluationResult r) {
        Values.sendEvaluated(exceptionWriter, r);
    }

    ## Most important function: actual execution happens here.
    void execute() throws Exception {
        ## In Java, we must execute_module the before and after code in the context.
        ${before}

        ## Call the main function if needed.
        this.writeSeparator();
        % if context_testcase.exists:
            try {
                ${submission_name}.main(new String[]{
                % for argument in context_testcase.arguments:
                    "${argument}", \
                % endfor
                });
                <%include file="statement.mako" args="statement=context_testcase.exception_statement()" />;
            } catch (Exception | AssertionError e) {
                <%include file="statement.mako" args="statement=context_testcase.exception_statement('e')" />;
            }
        % endif

        ## Generate the actual tests based on the context.
        % for testcase in testcases:
            ## In Java, we need special code to make variables available outside of
            ## the try-catch block.
            this.writeSeparator();
            % if isinstance(testcase.command, get_args(Assignment)):
                <%include file="declaration.mako" args="tp=testcase.command.type,value=testcase.command.expression" /> ${testcase.command.variable} = null;
            % endif
            try {
                <%include file="statement.mako" args="statement=testcase.input_statement()" />;
                <%include file="statement.mako" args="statement=testcase.exception_statement()" />;
            } catch (Exception | AssertionError e) {
                <%include file="statement.mako" args="statement=testcase.exception_statement('e')" />;
            }
        % endfor

        ${after}
    }

    @Override
    public void close() throws IOException {
        this.valueWriter.close();
        this.exceptionWriter.close();
    }

    public static void main(String[] a) throws Exception {
        try(${context_name} context = new ${context_name}()) {
            context.execute();
        }
    }
}
