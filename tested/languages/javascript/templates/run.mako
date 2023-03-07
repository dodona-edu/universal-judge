## Code to execute one context.
<%! from tested.languages.generator import _TestcaseArguments %>\
<%! from tested.serialisation import Statement, Expression, Assignment %>\
<%! from tested.utils import get_args %>\
const fs = require('fs');
const values = require("./values.js");

##################################
## Setup                        ##
##################################

## Import the language specific evaluators we will need.
% for name in evaluator_names:
    const ${name} = require("./${name}.js");
% endfor

## Open the files to which we write results.
const valueFile = fs.openSync("${value_file}", "w");
const exceptionFile = fs.openSync("${exception_file}", "w");

## Write the separator and flush to ensure the output is in the files.
## This is necessary, otherwise the separators are sometimes missing when
## execution is killed due to timeouts.
function writeSeparator() {
    fs.writeSync(valueFile, "--${secret_id}-- SEP");
    fs.writeSync(exceptionFile, "--${secret_id}-- SEP");
    fs.writeSync(process.stdout.fd, "--${secret_id}-- SEP");
    fs.writeSync(process.stderr.fd, "--${secret_id}-- SEP");
}

function writeContextSeparator() {
    fs.writeSync(valueFile, "--${context_secret_id}-- SEP");
    fs.writeSync(exceptionFile, "--${context_secret_id}-- SEP");
    fs.writeSync(process.stdout.fd, "--${context_secret_id}-- SEP");
    fs.writeSync(process.stderr.fd, "--${context_secret_id}-- SEP");
}

##################################
## Predefined functions         ##
##################################

## Send a value to TESTed.
async function sendValue(value) {
    values.sendValue(valueFile, await value);
}

## Send an exception to TESTed.
async function sendException(exception) {
    values.sendException(exceptionFile, await exception);
}

## Send the result of a language specific value evaluator to TESTed.
async function sendSpecificValue(value) {
    values.sendEvaluated(valueFile, await value);
}

## Send the result of a language specific exception evaluator to TESTed.
async function sendSpecificException(exception) {
    values.sendEvaluated(exceptionFile, await exception);
}


(async () => {

% for i, ctx in enumerate(contexts):
    async function context${i}() {
        ${ctx.before}

        ## Import the code if there is no main testcase.
        % if not ctx.context.has_main_testcase():
            delete require.cache[require.resolve("./${submission_name}.js")];
            const ${submission_name} = require("./${submission_name}.js");
        % endif

        % for tc in ctx.testcases:
            writeSeparator();

            ## Prepare the command line arguments if needed.
            % if tc.testcase.is_main_testcase():
                let new_args = [process.argv[0]];
                new_args = new_args.concat([\
                    % for argument in tc.input.arguments:
                        "${argument}", \
                    % endfor
                ]);
                process.argv = new_args;
            % endif

            ## In JavaScript, we need special code to make variables available outside of
            ## the try-catch block.
            % if not tc.testcase.is_main_testcase() and isinstance(tc.input.command, get_args(Assignment)):
                let ${tc.input.command.variable};
            % endif

            try {

                % if tc.testcase.is_main_testcase():
                    ## If it is a main tc, import the code, which will call the main function.
                    delete require.cache[require.resolve("./${submission_name}.js")];
                    const ${submission_name} = require("./${submission_name}.js");
                % else:
                    ## If we have a value function, we have an expression.
                    <%include file="statement.mako" args="statement=tc.input.input_statement()" />;
                % endif

                <%include file="statement.mako" args="statement=tc.exception_statement()" />;
            } catch(e) {
                <%include file="statement.mako" args="statement=tc.exception_statement('e')" />;
            }
        % endfor
        ${ctx.after}
    }
% endfor



% for i, ctx in enumerate(contexts):
    writeContextSeparator();
    await context${i}();
% endfor

## Close output files.
fs.closeSync(valueFile);
fs.closeSync(exceptionFile);

})();
