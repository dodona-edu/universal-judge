## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.
public class Context {

    private final Evaluator evaluator;

    private Context(Evaluator evaluator) {
        this.evaluator = evaluator;
    }

    private void execute() throws Exception {
        ## In Java, we must execute the before and after code in the context.
        ${before}

        ## Call the main fucnction if necessary
        % if main_testcase.exists:
            try {
                ${submission_name}.main(new String[]{
                % for argument in main_testcase.arguments:
                    <%include file="value.mako" args="value=argument"/>
                    % if not loop.last:
                        , \
                    % endif
                % endfor
                });
            } catch (Exception e) {
                evaluator.e_evaluate_main(e);
            }
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            evaluator.writeDelimiter("--${secret_id}-- SEP");
        % endif

        % for additional in additional_testcases:
            try {
                % if additional.has_return:
                    System.out.println("Hallo");
                    evaluator.v_evaluate_${loop.index}(<%include file="function.mako" args="function=additional.function" />);
                % else:
                    <%include file="function.mako" args="function=additional.function" />;
                % endif
            } catch (Exception e) {
                evaluator.e_evaluate_${loop.index}(e);
            }
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            evaluator.writeDelimiter("--${secret_id}-- SEP");

        % endfor

        ${after}
    }

    public static void main(String[] a) throws Exception {
        ## Open our file we use to write.
        Evaluator evaluator = new Evaluator();
        new Context(evaluator).execute();
        evaluator.close();
    }
}
