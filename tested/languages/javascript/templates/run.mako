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
function sendValue(value) {
    values.sendValue(valueFile, value);
}

## Send an exception to TESTed.
function sendException(exception) {
    values.sendException(exceptionFile, exception);
}

## Send the result of a language specific value evaluator to TESTed.
function sendSpecificValue(value) {
    values.sendEvaluated(valueFile, value);
}

## Send the result of a language specific exception evaluator to TESTed.
function sendSpecificException(exception) {
    values.sendEvaluated(exceptionFile, exception);
}


(async () => {

let ${submission_name};

## Prepare the command line arguments if needed.
% if run_testcase.exists:
    let new_args = [process.argv[0]]
    new_args = new_args.concat([\
    % for argument in run_testcase.arguments:
        "${argument}", \
    % endfor
    ])
    process.argv = new_args
% endif
## Import the code for the first time, which will run the code.
try {
    writeContextSeparator();
    ${submission_name} = require("./${submission_name}.js");

    <%include file="statement.mako" args="statement=run_testcase.exception_statement()" />
} catch(e) {
    ## If there is a main test case, pass the exception to it.
    <%include file="statement.mako" args="statement=run_testcase.exception_statement('e')" />
}

% for i, ctx in enumerate(contexts):
    async function context${i}() {
        ${ctx.before}
        % for testcase in ctx.testcases:
            writeSeparator();
            <% testcase: _TestcaseArguments %>\
            % if isinstance(testcase.command, get_args(Assignment)):
                let ${testcase.command.variable};
            % endif
            try {
                ## If we have a value function, we have an expression.
                <%include file="statement.mako" args="statement=testcase.input_statement()" />;

                <%include file="statement.mako" args="statement=testcase.exception_statement()" />;
            } catch(e) {
                <%include file="statement.mako" args="statement=testcase.exception_statement('e')" />;
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
