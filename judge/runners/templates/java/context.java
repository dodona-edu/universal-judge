## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.
<%! from testplan import BuiltinEvaluator %>

import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Collections;
import java.util.Collection;

class Context${context_id} {

    private final FileWriter ${secret_id}_writer;

    private Context${context_id}(FileWriter writer) {
        this.${secret_id}_writer = writer;
    }

    private void evaluated(boolean result, String expected, String actual, Collection<String> messages) throws Exception {
        Values.evaluated(this.${secret_id}_writer, result, expected, actual, messages);
    }

    private void evaluated(boolean result, String expected, String actual) throws Exception {
        this.evaluated(result, expected, actual, Collections.emptyList());
    }

    private void send(Object value) throws Exception {
        Values.send(this.${secret_id}_writer, value);
    }

    % for additional in additionals:
        private void eval_${context_id}_${loop.index}(Object value) throws Exception {
            ${additional.value_code}
        }
    % endfor

    private void execute() throws Exception {
        ## In Java, we must execute the before and after code in the context.
        ${before}

        ## Call the main fucnction.
        <%include file="function.mako" args="function=execution" />;

        % for additional in additionals:
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            ${secret_id}_writer.write("--${secret_id}-- SEP");
            eval_${context_id}_${loop.index}(${submission_name}.<%include file="function.mako" args="function=additional.function" />);
        % endfor

        ${after}
    }

    public static void main(String[] a) throws Exception {
        ## Open our file we use to write.
        FileWriter ${secret_id}_writer = new FileWriter("${output_file}");
        new Context${context_id}(${secret_id}_writer).execute();
        ${secret_id}_writer.close();
    }
}
