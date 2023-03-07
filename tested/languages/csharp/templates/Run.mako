## Code to execute one context.
<%! from tested.languages.generator import _TestcaseArguments %>\
<%! from tested.serialisation import Statement, Expression, Assignment %>\
<%! from tested.utils import get_args %>\
using System;
using System.IO;

namespace Tested
{
    class ${execution_name}
    {
        ##################################
        ## Setup                        ##
        ##################################

        ## Open the files to which we write results.
        private readonly StreamWriter valueFile;
        private readonly StreamWriter exceptionFile;
        private readonly StreamWriter stdout;
        private readonly StreamWriter stderr;

        public ${execution_name}()
        {
            valueFile = new StreamWriter(File.OpenWrite("${value_file}"));
            exceptionFile = new StreamWriter(File.OpenWrite("${exception_file}"));
            stdout = new StreamWriter(Console.OpenStandardOutput());
            stderr = new StreamWriter(Console.OpenStandardError());

            stdout.AutoFlush = true;
            stderr.AutoFlush = true;
            Console.SetOut(stdout);
            Console.SetError(stderr);
        }

        ## Write the separator and flush to ensure the output is in the files.
        ## This is necessary, otherwise the separators are sometimes missing when
        ## execution is killed due to timeouts.
        private void WriteSeparator() {
            valueFile.Write("--${secret_id}-- SEP");
            exceptionFile.Write("--${secret_id}-- SEP");
            stdout.Write("--${secret_id}-- SEP");
            stderr.Write("--${secret_id}-- SEP");
        }

        private void WriteContextSeparator() {
            valueFile.Write("--${context_secret_id}-- SEP");
            exceptionFile.Write("--${context_secret_id}-- SEP");
            stdout.Write("--${context_secret_id}-- SEP");
            stderr.Write("--${context_secret_id}-- SEP");
        }

        ##################################
        ## Predefined functions         ##
        ##################################

        ## Send a value to TESTed.
        private void SendValue(object? value)
        {
            Values.WriteValue(valueFile, value);
        }

        private void SendException(Exception? e)
        {
            Values.WriteException(exceptionFile, e);
        }

        private void SendSpecificValue(EvaluationResult value)
        {
            Values.SendEvaluated(valueFile, value);
        }

        private void SendSpecificException(EvaluationResult exception)
        {
            Values.SendEvaluated(exceptionFile, exception);
        }

        % for i, ctx in enumerate(contexts):
            private void Context${i}() {
                ${ctx.before}

                % for tc in ctx.testcases:
                    WriteSeparator();

                    ## Make a variable available outside of the try-catch block.
                    % if not tc.testcase.is_main_testcase() and isinstance(tc.input.command, get_args(Assignment)):
                        <%include file="declaration.mako" args="tp=tc.input.command.type, value=tc.input.command.expression" /> ${tc.input.command.variable} = null;
                    % endif

                    try {
                        % if tc.testcase.is_main_testcase():
                            ${submission_name}.Main(new string[]{\
                                % for argument in tc.input.arguments:
                                    "${argument}", \
                                % endfor
                            });
                        % else:
                            ## If we have a value function, we have an expression.
                            <%include file="statement.mako" args="statement=tc.input.input_statement()" />;
                        % endif
                        <%include file="statement.mako" args="statement=tc.exception_statement()" />;
                    } catch(System.Exception E) {
                        <%include file="statement.mako" args="statement=tc.exception_statement('e')" />;
                    }
                % endfor
                ${ctx.after}
            }
        % endfor

        void Execute()
        {
          % for i, ctx in enumerate(contexts):
              WriteContextSeparator();
              Context${i}();
          % endfor

          ## Close output files.
          valueFile.Close();
          exceptionFile.Close();
        }

        public static void Main(string[] args)
        {
            ${execution_name} execution = new ${execution_name}();
            execution.Execute();
        }
    }
}
