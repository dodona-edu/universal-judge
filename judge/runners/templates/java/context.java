## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.
<%! from testplan import BuiltinEvaluator %>

class Context${context_id} {

    private final Evaluator${context_id} evaluator;

    private Context${context_id}(Evaluator${context_id} evaluator) {
        this.evaluator = evaluator;
    }

    private void execute() throws Exception {
        ## In Java, we must execute the before and after code in the context.
        ${before}

        ## Call the main fucnction.
        ${submission_name}.main(new String[]{
            % for argument in main_arguments:
                <%include file="value.mako" args="value=argument"/>
                % if not loop.last:
                    , \
                % endif
            % endfor
        });

        % for additional in additionals:
            System.err.print("--${secret_id}-- SEP");
            System.out.print("--${secret_id}-- SEP");
            evaluator.valueWrite("--${secret_id}-- SEP");
            % if additional.has_return:
                evaluator.evaluate_${context_id}_${loop.index}(<%include file="function.mako" args="function=additional.function" />);
            % else:
                <%include file="function.mako" args="function=additional.function" />;
            % endif

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
