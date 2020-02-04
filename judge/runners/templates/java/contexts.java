## Code to execute one test context.
<%! from testplan import Assignment %>

public class Contexts {

    private final Evaluator evaluator;

    private Contexts(Evaluator evaluator) {
        this.evaluator = evaluator;
    }

    % for c in contexs:
        private void execute_${loop.index}() throws Exception {
            ## In Java, we must execute the before and after code in the context.
            ${c.before}

            ## Call the main fucnction if necessary
            % if main_testcase.exists:
                try {
                    ${submission_name}.main(new String[]{
                    % for argument in c.main_testcase.arguments:
                        <%include file="value.mako" args="value=argument"/>
                        % if not loop.last:
                            , \
                        % endif
                    % endfor
                    });
                } catch (Exception e) {
                    evaluator.e_evaluate_main_${loop.index}(e);
                }
                System.err.print("--${secret_id}-- SEP");
                System.out.print("--${secret_id}-- SEP");
                evaluator.writeDelimiter("--${secret_id}-- SEP");
            % endif

            <% c_number = loop.index %>
            % for additional in c.additional_testcases:
                % if isinstance(additional.statement, Assignment):
                    <%include file="declaration.mako" args="value=additional.statement.get_type()" /> ${additional.statement.name};
                % endif
                try {
                    % if isinstance(additional.statement, Assignment):
                        <%include file="assignment.mako" args="assignment=additional.statement" />
                    % else:
                        % if additional.has_return:
                            evaluator.v_evaluate_${c_number}_${loop.index}(\
                        % endif
                        <%include file="function.mako" args="function=additional.statement" />\
                        % if additional.has_return:
                            )\
                        % endif
                        ;
                    % endif
                } catch (Exception e) {
                    evaluator.e_evaluate_${c_number}_${loop.index}(e);
                }
                System.err.print("--${secret_id}-- SEP");
                System.out.print("--${secret_id}-- SEP");
                evaluator.writeDelimiter("--${secret_id}-- SEP");

            % endfor

            ${c.after}
        }
    % endfor

    public static void main(String[] a) throws Exception {
        var number = Integer.parse(a[1])
        var evaluator = new Evaluator();
        var context = new Context(evaluator);
        switch (number) {
            % for c in contexts:
                case ${loop.index}:
                    context.evaluate_${loop.index}();
                    break;
            % endfor
        }
        evaluator.close();
    }
}
