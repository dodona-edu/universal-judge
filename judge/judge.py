# Executor for exercises where stdin expects input and receives output in stdout.
from dataclasses import dataclass
from typing import List

from comparator import Comparator, FileComparator, TextComparator
from dodona import common as co, partial_output as po
from dodona.dodona import report_update
from jupyter import JupyterContext, KernelQueue
from runners.python import PythonRunner
from tested import Config
from testplan import _get_input, _get_stderr, _get_stdout, Evaluator, EvaluatorType, Plan, Run, Test, TestPlanError, \
    TEXT_COMPARATOR, Context
from utils.ascii_to_html import ansi2html


def _get_evaluator(evaluator: Evaluator) -> Comparator:
    """Get the evaluator for a test"""
    if evaluator.type == EvaluatorType.builtin:
        if evaluator.name == TEXT_COMPARATOR:
            return TextComparator(arguments=evaluator.options)
        elif evaluator.name == "fileComparator":
            return FileComparator(arguments=evaluator.options)
        else:
            raise TestPlanError(f"Unknown buil-in evaluator: {evaluator.name}")
    elif evaluator.type == 'external':
        raise NotImplementedError()


RUN_KERNELS = {
    'java': '\n{}.main(new String[]{{}})'
}


def needs_run(kernel: str) -> bool:
    return kernel in RUN_KERNELS


def create_run(kernel: str, run: Run) -> str:
    return RUN_KERNELS[kernel].format(run.classname)


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

    def _execute_test_plan(self, submission: str, test_plan: Plan):
        """
        Execute a test plano.

        :param test_plan: The plano to execute.
        :param submission: The code submitted by the user.
        """
        raise NotImplementedError()

    def judge(self, plan):
        """Get and execute the test plano for an exercise, resulting in a judgment."""
        with open(self.config.source, 'r') as file:
            submission_code = file.read()

        self._execute_test_plan(submission_code, plan)


class KernelJudge(Judge):
    """
    Execute exercises using Jupyter kernels.
    """

    @staticmethod
    def _execute_statement(code, input_, context: JupyterContext, timeout, memory_limit) -> ExecutionResult:
        """Execute user_code."""
        # print("Running:")
        # print(code)
        # print(input_)
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
                messages.append(
                    co.ExtendedMessage(description='<pre>' + '<br>'.join(coloured) + '</pre>', format='html'))
            elif error['ename'] in ('StdError', 'TooMuchInput'):
                stderr_.append(error['evalue'])
                error_codes.append(po.Status.WRONG)
            else:
                print()
                print(f"Unknown error: {error}")

        return ExecutionResult(stdout_, stderr_, error_codes, messages)

    def _execute_test(self, submission: str, test: Test, context: JupyterContext):
        """
        Run a single test.
        :param submission: The code to run the test against.
        :param test: The test to run.
        :param context: The context to run it in.
        :return: If the context should be restarted (i.e. cannot be re-used).
        """
        # Get the expected input for a test.
        try:
            input_ = _get_input(test)
        except TestPlanError as e:
            report_update(po.StartTest(""))
            report_update(po.AppendMessage(co.ExtendedMessage(
                description=str(e),
                format='text',
                permission=co.Permission.STAFF
            )))
            report_update(po.CloseTest("Internal error", po.StatusMessage(po.Status.INTERNAL_ERROR)))
            return
        report_update(po.StartTest("\n".join(input_)))
        try:
            stdout_evaluator = _get_evaluator(test.evaluators.stdout)
            stderr_evaluator = _get_evaluator(test.evaluators.stderr)
            file_evaluator = _get_evaluator(test.evaluators.file)
        except TestPlanError as e:
            report_update(po.AppendMessage(co.ExtendedMessage(
                description=str(e),
                format='text',
                permission=co.Permission.STAFF
            )))
            report_update(po.CloseTest("Internal error", po.StatusMessage(po.Status.INTERNAL_ERROR)))
            return

        if needs_run(self.config.kernel):
            run = create_run(self.config.kernel, test.runArgs)
            submission += run

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
            actual = ""
            success = True
            # Evaluate the stdout channel
            stdout_ = _get_stdout(test)
            if stdout_ is not None:  # We evaluate the stdout
                success = stdout_evaluator.evaluate(stdout_, result.stdout)

            # Evaluate the stderr channel
            stderr_ = _get_stderr(test)
            if stderr_ is not None:  # Check stderr
                success = success and stderr_evaluator.evaluate(stderr_, result.stderr)

            # Evaluate the file channel (if present)
            if test.output.file:
                actual = test.output.file.actual
                success = success and file_evaluator.evaluate(test.output.file.expected, actual)

            status = po.Status.CORRECT if success else po.Status.WRONG

        # Write messages
        for message in result.messages:
            report_update(po.AppendMessage(message=message))

        report_update(po.CloseTest(actual, po.StatusMessage(status), data=po.TestData(channel="stdout")))

    def _execute_test_plan(self, submission: str, test_plan: Plan):
        """Execute a test plano"""

        # Start a pool of kernels.
        kernels = KernelQueue(kernel=self.config.kernel)
        current_kernel = kernels.get_kernel(None)

        # TODO: when should contexts be cleared?
        report_update(po.StartJudgment())
        for tab in test_plan.tabs:
            report_update(po.StartTab(title=tab.name))
            for context in tab.contexts:
                report_update(po.StartContext(context.description))
                for testcase in context.testcases:
                    report_update(po.StartTestcase(testcase.description))
                    for test in testcase.tests:
                        self._execute_test(submission, test, current_kernel)
                    report_update(po.CloseTestcase())
                report_update(po.CloseContext())
                # print("Stopping context")
                current_kernel = kernels.get_kernel(current_kernel)
            report_update(po.CloseTab())
        report_update(po.CloseJudgment())

        # print("Clean kernels...")
        kernels.clean()
        # print("Kernels are cleaned")
        current_kernel.clean()
        # print("Cleaned")


class GeneratorJudge(Judge):

    def _execute_test_plan(self, submission: str, test_plan: Plan):

        report_update(po.StartJudgment())
        # Generate test files.
        runner = PythonRunner(self.config)
        ids = runner.generate_code(submission, test_plan)

        for tab in test_plan.tabs:
            report_update(po.StartTab(title=tab.name))
            for id_, context in zip(ids, tab.contexts):
                report_update(po.StartContext(context.description))
                try:
                    result = runner.execute(id_, context)
                except TestPlanError as e:
                    report_update(po.AppendMessage(co.ExtendedMessage(
                        description=str(e),
                        format='text',
                        permission=co.Permission.STAFF
                    )))
                    report_update(po.CloseTest("Internal error",
                                               po.StatusMessage(po.Status.INTERNAL_ERROR)))
                    continue
                self._process_results(context, result)
                report_update(po.CloseContext())
            report_update(po.CloseTab())
        report_update(po.CloseJudgment())

    def _process_results(self, context: Context, results: ExecutionResult):

        for i, testcase in enumerate(context.all_testcases()):
            input_ = _get_input(testcase)
            report_update(po.StartTestcase("\n".join(input_)))  # TODO: fancy input
            test = testcase.tests[0]  # We assume only one test per testcase.
            report_update(po.StartTest("\n".join(input_)))  # TODO: fancy input
            try:
                stdout_evaluator = _get_evaluator(test.evaluators.stdout)
                stderr_evaluator = _get_evaluator(test.evaluators.stderr)
                file_evaluator = _get_evaluator(test.evaluators.file)
            except TestPlanError as e:
                report_update(po.AppendMessage(co.ExtendedMessage(
                    description=str(e),
                    format='text',
                    permission=co.Permission.STAFF
                )))
                report_update(po.CloseTest("Internal error", po.StatusMessage(po.Status.INTERNAL_ERROR)))
                continue

            actual = results.stdout[i]
            success = True
            # TODO: allow test on return value.
            # Evaluate the stdout channel
            stdout_ = _get_stdout(test)
            if stdout_ is not None:  # We evaluate the stdout
                success = stdout_evaluator.evaluate(stdout_, results.stdout[i])
            # Evaluate the stderr channel
            stderr_ = _get_stderr(test)
            if stderr_ is not None:  # Check stderr
                success = success and stderr_evaluator.evaluate(stderr_, results.stderr[i])
            # Evaluate the file channel (if present)
            if test.output.file:
                actual = test.output.file.actual
                success = success and file_evaluator.evaluate(test.output.file.expected, actual)

            status = po.Status.CORRECT if success else po.Status.WRONG
            # TODO: report data channel based on actual tests.
            report_update(po.CloseTest(actual, po.StatusMessage(status), data=po.TestData(channel="stdout")))
            report_update(po.CloseTestcase())
