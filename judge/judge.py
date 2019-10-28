# Executor for exercises where stdin expects input and receives output in stdout.
import json
import traceback
from dataclasses import dataclass
from os import path
from typing import List

from comparator import Comparator, FileComparator, TextComparator, ValueComparator
from dodona import common as co, partial_output as po
from dodona.common import ExtendedMessage, Permission, supports_input_highlighting
from dodona.dodona import report_update
from jupyter import JupyterContext, KernelQueue
from runners.common import ExecutionResult, Runner
from runners.python import PythonRunner
from tested import Config
from testplan import _get_stderr, _get_stdin, _get_stdout, Context, Evaluator, EvaluatorType, FILE_COMPARATOR, \
    FunctionType, OutputChannelState, Plan, Testcase, TestPlanError, TEXT_COMPARATOR, VALUE_COMPARATOR
from utils.ascii_to_html import ansi2html


def _get_or_default(seq, i, default):
    try:
        return seq[i]
    except IndexError:
        return default


def _get_evaluator(evaluator: Evaluator) -> Comparator:
    """Get the evaluator instance from a test plan."""
    if evaluator.type == EvaluatorType.builtin:
        if evaluator.name == TEXT_COMPARATOR:
            return TextComparator(arguments=evaluator.options)
        elif evaluator.name == FILE_COMPARATOR:
            return FileComparator(arguments=evaluator.options)
        elif evaluator.name == VALUE_COMPARATOR:
            return ValueComparator(arguments=evaluator.options)
        else:
            raise TestPlanError(f"Unknown buil-in evaluator: {evaluator.name}")
    elif evaluator.type == 'external':
        raise NotImplementedError()


def _evaluate_channel(channel: str, expected, actual, evaluator: Comparator,
                      error_message: List[co.ExtendedMessage]):
    """
    Evaluate the output on a given channel. This function will output the appropriate messages
    to start and end a new test in Dodona.

    If errors is given, the test will end with a runtime error.

    :param channel: The name of the channel being evaluated. Will be displayed in Dodona.
    :param expected: The expected value, or None to skip evaluation.
    :param actual: The actual output. Ignored if expected is None.
    :param evaluator: The evaluator to use.
    :param error_message: The potential errors.
    """
    if expected is None:
        return  # Nothing to do
    report_update(po.StartTest(evaluator.get_readable_input(expected)))
    if error_message:
        for m in error_message:
            report_update(po.AppendMessage(m))
        status = po.Status.RUNTIME_ERROR
    else:
        success = evaluator.evaluate(expected, actual)
        status = po.Status.CORRECT if success else po.Status.WRONG
    if actual is None:
        actual = ""

    report_update(po.CloseTest(actual, po.StatusMessage(status), data=po.TestData(channel)))


RUN_KERNELS = {
    'java': '\n{}.execution(new String[]{{}})'
}


def needs_run(kernel: str) -> bool:
    return kernel in RUN_KERNELS


@dataclass
class KernelResult:
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
    def _execute_statement(code, input_, context: JupyterContext, timeout, memory_limit) -> KernelResult:
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

        return KernelResult(stdout_, stderr_, error_codes, messages)

    def _execute_test(self, submission: str, test: Testcase, context: JupyterContext):
        """
        Run a single test.
        :param submission: The code to run the test against.
        :param test: The test to run.
        :param context: The context to run it in.
        :return: If the context should be restarted (i.e. cannot be re-used).
        """
        # Get the expected input for a test.
        try:
            input_ = _get_stdin(test)
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
            run = ""  # create_run(self.config.kernel, test.runArgs)
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
                success = stdout_evaluator.evaluate(stdout_, "\n".join(result.stdout))

            # Evaluate the stderr channel
            stderr_ = _get_stderr(test)
            if stderr_ is not None:  # Check stderr
                success = success and stderr_evaluator.evaluate(stderr_, "\n".join(result.stderr))

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
                current_kernel = kernels.get_kernel(current_kernel)
            report_update(po.CloseTab())
        report_update(po.CloseJudgment())

        kernels.clean()
        current_kernel.clean()


def _get_readable_input(case: Testcase, runner: Runner) -> ExtendedMessage:
    """
    Get human readable input for a testcase. This function will use, in order of availability:

    1. A description on the testcase.
    2. A function call. If the function call is the main function, only use the main function if
       the language requires a main function. However, if there is no stdin but there is a main
       function for a language that does not use it, a placeholder will be used.
    3. The stdin.

    If the input is code, the message type will be set to code or the language's name,
    if Dodona has support for the language.

    :param case: The testcase to get the input from.
    :param runner: Used to generate a function if the input is a function.
    """
    input_ = case.input
    format_ = 'text'  # By default, we use text as input.
    if case.description:
        text = case.description
    elif input_.function and (input_.function.type != FunctionType.main or runner.needs_main()):
        text = runner.function_call(input_.function)
        if supports_input_highlighting(runner.config.programming_language):
            format_ = runner.config.programming_language
        else:
            format_ = 'code'
    elif input_.stdin:
        text = _get_stdin(case)
    else:
        # If there is no stdin, but there is a main function but we end up here, this means the
        # language does not use main functions. In that case, we use a placeholder.
        if input_.function and input_.function.type == FunctionType.main:
            text = "Code execution"
        else:
            raise TestPlanError("Testcase without either description, stdin or function is not allowed.")
    return ExtendedMessage(description=text, format=format_)


class GeneratorJudge(Judge):

    runner: Runner

    def __init__(self, config: Config):
        super().__init__(config)
        self.runner = PythonRunner(self.config)

    def _execute_test_plan(self, submission: str, test_plan: Plan):
        report_update(po.StartJudgment())
        # Generate test files.
        ids = self.runner.generate_code(submission, test_plan)

        for tab in test_plan.tabs:
            report_update(po.StartTab(title=tab.name))
            for id_, context in zip(ids, tab.contexts):
                report_update(po.StartContext(context.description))
                try:
                    result = self.runner.execute(id_, context)
                except TestPlanError as e:
                    report_update(po.AppendMessage(co.ExtendedMessage(
                        description=str(e),
                        format='text',
                        permission=co.Permission.STAFF
                    )))
                    report_update(po.CloseTest("", po.StatusMessage(po.Status.INTERNAL_ERROR)))
                    continue
                self._process_results(context, result)
                report_update(po.CloseContext())
            report_update(po.CloseTab())
        report_update(po.CloseJudgment())

    def _process_results(self, context: Context, results: ExecutionResult):
        # Process output
        stdout_ = results.stdout.split(results.separator)
        stderr_ = results.stderr.split(results.separator)
        values = results.results.split(results.separator)

        # There might be less output than testcase, which is an error. However, we process the
        # output we have, to ensure we send as much feedback as possible to the user.

        for i, testcase in enumerate(context.all_testcases()):

            try:
                stdout_evaluator = _get_evaluator(testcase.evaluators.stdout)
                stderr_evaluator = _get_evaluator(testcase.evaluators.stderr)
                file_evaluator = _get_evaluator(testcase.evaluators.file)
                result_evaluator = _get_evaluator(testcase.evaluators.result)
            except TestPlanError as e:
                report_update(po.AppendMessage(co.ExtendedMessage(str(e), 'text', co.Permission.STAFF)))
                break

            readable_input = _get_readable_input(testcase, self.runner)
            report_update(po.StartTestcase(readable_input))

            error_message: List[ExtendedMessage] = []

            # Evaluate the file channel.
            # We evaluate this channel early, since it is separate from the other channels.
            expected_file = getattr(testcase.output.file, 'expected', None)
            actual_file = getattr(testcase.output.file, 'expected', None)
            if expected_file and not path.exists(expected_file):
                raise TestPlanError(f"Expected file {expected_file} not found.")
            if actual_file and not path.exists(actual_file):
                error_message.append(co.ExtendedMessage(f"File {actual_file} does not exist.", 'text'))
            _evaluate_channel("file", expected_file, actual_file, file_evaluator, error_message)

            error_message.clear()  # Reset, since there might be file errors.

            # Check if there is early termination.
            if i >= len(stdout_) or i >= len(stderr_) or i >= len(values):
                assert i >= len(stdout_) and i >= len(stderr_) and i >= len(values)
                error_message.append(co.ExtendedMessage("Tests were terminated early.", 'text'))

            # Evaluate the error channel.
            # If we expect no errors, we produce an error message, which is used in subsequent checks.
            # However, if we do expected something on this channel, we treat it as a normal channel.
            actual_stderr = stderr_[i] if i < len(stderr_) else None
            if testcase.output.stderr == OutputChannelState.none:
                # Use it as an error message, if it exists.
                if actual_stderr:
                    error_message.append(co.ExtendedMessage(actual_stderr, 'code'))
            else:
                # Use it as a normal channel.
                _evaluate_channel("stderr", _get_stderr(testcase), actual_stderr, stderr_evaluator, error_message)

            # Evaluate the stdout channel.
            actual_stdout = stdout_[i] if i < len(stdout_) else None
            _evaluate_channel("stdout", _get_stdout(testcase), actual_stdout, stdout_evaluator, error_message)

            # Evaluate value channel
            try:
                actual_return = json.loads(values[i]) if i < len(stdout_) else None
            except (ValueError, TypeError):
                actual_return = None
                error_message.append(co.ExtendedMessage(traceback.format_exc(), 'code', Permission.STAFF))
                error_message.append(co.ExtendedMessage("Internal error while reading return value.", 'text'))

            _evaluate_channel("return", testcase.output.result, actual_return, result_evaluator, error_message)

            report_update(po.CloseTestcase())
