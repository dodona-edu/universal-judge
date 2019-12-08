## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.


var evaluator = new Evaluator();

${before}

## Call the main fucnction if necessary
/open ${submission_name}.jsh
% if main_testcase.exists:
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
