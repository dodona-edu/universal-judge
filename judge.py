# Executor for exercises where stdin expects input and receives output in stdout.
import glob
from dataclasses import dataclass
from typing import List
from pathlib import Path

from comparator import Comparator, TextComparator, FileComparator, NothingComparator
from testplan import Plan, TestPlanError, Test, parse_test_plan
from utils.ascii_to_html import ansi2html

from tested import Config
from dodona import partial_output as po
from dodona import common as co
from dodona.dodona import report_update
from jupyter import JupyterContext


def __get_expected(test: Test) -> str:
    """Get the expected value of a test"""
    if test.input.stdin.type == "text":
        return test.input.stdin.data
    elif test.input.stdin.type == "file":
        with open(test.input.stdin.data, "r") as file:
            return file.read()
    else:
        raise TestPlanError(f"Unknown input type in test plan: {test.input.stdin.type}")


def __get_evaluator(test: Test) -> Comparator:
    """Get the evaluator for a test"""
    if not test.evaluator:  # Determine from output type
        if test.output.stdout:
            if test.output.stdout.type == 'text':
                return TextComparator()
            elif test.output.stdout.type == 'file':
                return TextComparator(expected_is_file=True)
            else:
                raise TestPlanError(f"Unknown stdout type in test plan: {test.output.stdout.type}")
        elif test.output.file:
            return FileComparator()
        else:
            return NothingComparator()
    elif test.evaluator.type == 'builtin':
        if test.evaluator.name == 'textComparator':
            if test.output.stdout and test.output.stdout.type == 'file':
                return TextComparator(expected_is_file=True, arguments=test.evaluator.options)
            else:
                return TextComparator(arguments=test.evaluator.options)
        elif test.evaluator.name == 'fileComparator':
            return FileComparator(arguments=test.evaluator.options)
        else:
            raise TestPlanError(f"Unknown buil-in evaluator: {test.evaluator.name}")
    elif test.evaluator.type == 'external':
        raise NotImplementedError()


@dataclass
class ExecutionResult:
    stdout: List[str]
    stderr: List[str]
    errors: List[po.Status]
    messages: List[po.Message]


class Judge:
    """Will evaluate an exercise."""

    config: Config

    def __init__(self, config: Config):
        self.config = config

    def _execute_test_plan(self, submission: str, test_plan: Plan, j_context: JupyterContext):
        """
        Execute a test plan.

        :param test_plan: The plan to execute.
        :param submission: The code submitted by the user.
        :param j_context: A context used to evaluate the user code.
        """
        raise NotImplementedError()

    def judge(self):
        """Get and execute the test plan for an exercise, resulting in a judgment."""
        with open(self.config.source, 'r') as file:
            submission_code = file.read()

        # For now we always want basic.json
        with open(f"{self.config.resources}/basic.json", 'r') as file:
            plan = parse_test_plan(file)

        self._execute_test_plan(submission_code, plan, JupyterContext(language=self.config.programming_language))


class KernelJudge(Judge):
    """
    Execute exercises using Jupyter kernels.
    """

    @staticmethod
    def _execute_statement(code, input_, context: JupyterContext, timeout, memory_limit) -> ExecutionResult:
        """Execute user_code."""
        messages = context.execute_statements(code, timeout, memory_limit, std_input=input_)
        # Collect stdout from messages.
        stdout_ = []
        errors_ = []
        error_codes = []

        # Handle messages from the kernel.
        for message in messages['iopub']:
            type_ = message['header']['msg_type']
            if type_ == 'stream':
                stream = message['content']['name']
                if stream == 'stdout':
                    stdout_.append(message['content']['text'])
                elif stream == 'stderr':
                    errors_.append({
                        'ename': 'StdError',
                        'evalue': message['content']['text']
                    })
                else:
                    raise ValueError(f"Unknown type {stream}")
            elif type_ == 'error' and message['content']['traceback']:
                errors_.append({
                    'ename': 'RuntimeError',
                    'evalue': message['content']['traceback']
                })
            else:
                print()
                print(f"Unknown message: {message}")

        # Handle messages from the client.
        for message in messages['client']:
            if message['msg_type'] == 'error':
                errors_.append(message['content'])
            else:
                print()
                print(f"Unknown message: {message}")

        stderr_ = []
        messages = []

        # Process error messages.
        for error in errors_:
            if error['ename'] == 'TimeoutError':
                error_codes.append(po.Status.TIME_LIMIT_EXCEEDED)
            elif error['ename'] == 'RuntimeError':
                error_codes.append(po.Status.RUNTIME_ERROR)
                coloured = [ansi2html(x) for x in error['evalue']]
                messages.append(co.ExtendedMessage(description='<pre>' + '<br>'.join(coloured) + '</pre>', format='html'))
            elif error['ename'] == 'StdError':
                stderr_.append(error['evalue'])
                error_codes.append(po.Status.WRONG)
            else:
                print()
                print(f"Unknown error: {error}")

        return ExecutionResult(stdout_, stderr_, error_codes, messages)

    def _execute_test(self, submission: str, test: Test, context: JupyterContext) -> bool:
        """
        Run a single test.
        :param submission: The code to run the test against.
        :param test: The test to run.
        :param context: The context to run it in.
        :return: If the context should be restarted (i.e. cannot be re-used).
        """
        # Get the expected input for a test.
        try:
            expected = test.get_expected()
        except TestPlanError as e:
            report_update(po.StartTest(""))
            report_update(po.AppendMessage(co.ExtendedMessage(
                description=str(e),
                format='text',
                permission=co.Permission.STAFF
            )))
            report_update(po.CloseTest("Internal error", po.StatusMessage(po.Status.INTERNAL_ERROR)))
            return False
        report_update(po.StartTest(expected))
        try:
            evaluator = test.get_evaluator()
        except TestPlanError as e:
            report_update(po.AppendMessage(co.ExtendedMessage(
                description=str(e),
                format='text',
                permission=co.Permission.STAFF
            )))
            report_update(po.CloseTest("Internal error", po.StatusMessage(po.Status.INTERNAL_ERROR)))
            return False

        input_ = test.input.stdin.data

        result = self._execute_statement(submission, input_, context,
                                         timeout=self.config.time_limit,
                                         memory_limit=self.config.memory_limit)

        if result.errors:
            actual = ""
            status = result.errors.pop()
            stderr = "\n".join(result.stderr)
            if stderr:
                result.messages.append(co.ExtendedMessage(description=stderr, format='text'))
        else:
            # Evaluate solution
            if test.output.stdout:  # Evaluate output from stdout
                actual = "\n".join(result.stdout)
                success = evaluator.evaluate(test.output.stdout.data, actual)
            elif test.output.file:  # Output must be a file, compare them
                actual = test.output.file.actual
                success = evaluator.evaluate(test.output.file.expected, actual)
            else:
                # This evaluator does nothing.
                actual = ""
                success = evaluator.evaluate("", actual)

            status = po.Status.CORRECT if success else po.Status.WRONG

        # Write messages
        for message in result.messages:
            report_update(po.AppendMessage(message=message))

        report_update(po.CloseTest(actual, po.StatusMessage(status), data=po.TestData(channel="stdout")))
        return False

    def _execute_test_plan(self, submission: str, test_plan: Plan, j_context: JupyterContext):
        """Execute a test plan"""

        # Start the Jupyter kernel.
        j_context.run()
        is_running = True

        # TODO: when should contexts be cleared?
        report_update(po.StartJudgment())
        for tab in test_plan.tabs:
            report_update(po.StartTab(title=tab.name))
            for context in tab.contexts:
                if not is_running:
                    j_context.run()
                report_update(po.StartContext(context.description))
                for testcase in context.testcases:
                    report_update(po.StartTestcase(testcase.description))
                    for test in testcase.tests:
                        if self._execute_test(submission, test, j_context):
                            j_context.clean()
                            j_context.start()
                    report_update(po.CloseTestcase())
                report_update(po.CloseContext())
                # TODO: kernels that don't support this.
                r = self._execute_statement("%reset", None, j_context, 100, self.config.memory_limit)
                if r.errors:
                    j_context.clean()
                    is_running = False
            report_update(po.CloseTab())
        report_update(po.CloseJudgment())

        if is_running:
            j_context.clean()
