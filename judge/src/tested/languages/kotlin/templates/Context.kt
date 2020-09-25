import Statement
import _TestcaseArguments
import get_args
import java.io.PrintWriter

## Code to execute_module one test context.
<%! from tested.languages.generator import _TestcaseArguments %>\
<%! from tested.serialisation import Statement, Expression, Assignment %>\
<%! from tested.utils import get_args %>

import java.io.PrintWriter

class ${context_name} : AutoCloseable {
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

    private fun sendValue(value: Any?) {
        valuesSend(valueWriter, value)
    }

    private fun sendException(throwable: Throwable) {
        valuesSendException(exceptionWriter, throwable)
    }

    private fun sendSpecificValue(value: EvaluationResult) {
        valuesSendEvaluated(valueWriter, value)
    }

    private fun sendSpecificException(exception: EvaluationResult) {
        valuesSendEvaluated(exceptionWriter, exception)
    }

    fun execute() {
        ${ before }

        this.writeSeparator()

        % if context_testcase.exists:
        try {
            solutionMain(arrayOf( \
                % for argument in context_testcase.arguments:
                    "${argument}", \
                % endfor
            ))
            <%include file = "statement.mako" args = "statement=context_testcase.exception_statement()" />
        } catch (e: Exception) {
            <%include file = "statement.mako" args = "statement=context_testcase.exception_statement('e')" />
        } catch (e: AssertionError) {
            <%include file = "statement.mako" args = "statement=context_testcase.exception_statement('e')" />
        }
        % endif

        % for testcase in testcases:
        this.writeSeparator();
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

        ${ after }
    }

    override fun close() {
        valueWriter.close()
        exceptionWriter.close()
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String> = emptyArray()) {
            val context = ${context_name}()
            try {
                context.execute();
            } finally {
                context.close()
            }
        }
    }
}
