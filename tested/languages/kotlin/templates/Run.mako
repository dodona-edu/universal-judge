## Code to execute_module one test context.
<%! from tested.languages.generator import _TestcaseArguments %>\
<%! from tested.serialisation import Statement, Expression, Assignment %>\
<%! from tested.utils import get_args %>

import java.io.PrintWriter
import java.math.BigInteger
import java.math.BigDecimal

class ${execution_name} : AutoCloseable {
    private val valueWriter = PrintWriter("${value_file}")
    private val exceptionWriter = PrintWriter("${exception_file}")

    private fun writeSeparator() {
        valueWriter.print("--${secret_id}-- SEP")
        valueWriter.flush()
        exceptionWriter.print("--${secret_id}-- SEP")
        exceptionWriter.flush()
        print("--${secret_id}-- SEP")
        System.out.flush()
        System.err.print("--${secret_id}-- SEP")
        System.err.flush()
    }

    private fun writeContextSeparator() {
        valueWriter.print("--${context_secret_id}-- SEP")
        valueWriter.flush()
        exceptionWriter.print("--${context_secret_id}-- SEP")
        exceptionWriter.flush()
        print("--${context_secret_id}-- SEP")
        System.out.flush()
        System.err.print("--${context_secret_id}-- SEP")
        System.err.flush()
    }

    private fun sendValue(value: Any?) {
        valuesSend(valueWriter, value)
    }

    private fun sendException(throwable: Throwable?) {
        valuesSendException(exceptionWriter, throwable)
    }

    private fun sendSpecificValue(value: EvaluationResult) {
        valuesSendEvaluated(valueWriter, value)
    }

    private fun sendSpecificException(exception: EvaluationResult) {
        valuesSendEvaluated(exceptionWriter, exception)
    }

    % for i, ctx in enumerate(contexts):
        private fun context${i}() {
            ${ctx.before}
            % for testcase in ctx.testcases:
                this.writeSeparator()
                % if isinstance(testcase.command, get_args(Assignment)):
                    var ${testcase.command.variable} : <%include file = "declaration.mako" args = "tp=testcase.command.type,value=testcase.command.expression" /> = null
                % endif
                try {
                    <%include file = "statement.mako" args = "statement=testcase.input_statement()" />
                    <%include file = "statement.mako" args = "statement=testcase.exception_statement()" />
                } catch (e: Exception) {
                    <%include file = "statement.mako" args = "statement=testcase.exception_statement('e')" />
                } catch (e: AssertionError) {
                    <%include file = "statement.mako" args = "statement=testcase.exception_statement('e')" />
                }
            % endfor
            ${ctx.after}
        }
    % endfor

    fun execute() {
        this.writeContextSeparator()

        % if run_testcase.exists:
            try {
                solutionMain(arrayOf( \
                    % for argument in run_testcase.arguments:
                        "${argument}", \
                    % endfor
                ))
                <%include file = "statement.mako" args = "statement=run_testcase.exception_statement()" />
            } catch (e: Exception) {
                <%include file = "statement.mako" args = "statement=run_testcase.exception_statement('e')" />
            } catch (e: AssertionError) {
                <%include file = "statement.mako" args = "statement=run_testcase.exception_statement('e')" />
            }
        % endif

        % for i, ctx in enumerate(contexts):
            this.writeContextSeparator()
            this.context${i}()
        % endfor
    }

    override fun close() {
        valueWriter.close()
        exceptionWriter.close()
    }
}

fun main(args: Array<String> = emptyArray()) {
    val execution = ${execution_name}()
    try {
        execution.execute();
    } finally {
        execution.close()
    }
}
