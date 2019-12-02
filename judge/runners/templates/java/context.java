## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.
class Context${context_id} {

    private final Evaluator${context_id} evaluator;

    private Context${context_id}(Evaluator${context_id} evaluator) {
        this.evaluator = evaluator;
    }

    private void execute() throws Exception {
        ## In Java, we must execute the before and after code in the context.
        ${before}

        ## Call the main fucnction if necessary
        % if execution.exists:
            try {
                ${submission_name}.main(new String[]{
                % for argument in execution.arguments:
                    <%include file="value.mako" args="value=argument"/>
                    % if not loop.last:
                        , \
                    % endif
                % endfor
                });
            } catch (Exception e) {
                evaluator.e_evaluate_execution(e);
            }
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            evaluator.writeDelimiter("--${secret_id}-- SEP");
        % endif

        % for additional in additionals:
            try {
                % if additional.has_return:
                    System.out.println("Hallo");
                    evaluator.v_evaluate_${context_id}_${loop.index}(<%include file="function.mako" args="function=additional.function" />);
                % else:
                    <%include file="function.mako" args="function=additional.function" />;
                % endif
            } catch (Exception e) {
                evaluator.e_evaluate_${context_id}_${loop.index}(e);
            }
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            evaluator.writeDelimiter("--${secret_id}-- SEP");

        % endfor

        ${after}
    }

    public static void main(String[] a) throws Exception {
        ## Open our file we use to write.
        Evaluator${context_id} evaluator = new Evaluator${context_id}();
        new Context${context_id}(evaluator).execute();
        evaluator.close();
    }
}
