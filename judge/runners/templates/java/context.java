## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.
<%! from testplan import BuiltinEvaluator %>

import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

class Context${context_id} {

    % for additional in additionals:
        public static void eval_${context_id}_${loop.index}(FileWriter output, Object value) throws Exception {
            % if isinstance(additional.output.result.evaluator, BuiltinEvaluator):
                Values.send(output, value);
            % endif
        }
    % endfor

    public static void main(String[] a) throws Exception {

        ## Open our file we use to write.
        FileWriter ${secret_id}_writer = new FileWriter("${output_file}");

        ## In Java, we must execute the before and after code in the context.
        ${before}

        ## Call the main fucnction.
        <%include file="function.mako" args="function=execution" />;

        % for additional in additionals:
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            ${secret_id}_writer.write("--${secret_id}-- SEP");
            eval_${context_id}_${loop.index}(${secret_id}_writer, ${submission_name}.<%include file="function.mako" args="function=additional.input.function" />);
        % endfor

        ${after}

        ${secret_id}_writer.close();
    }
}
