import json
import traceback
from os import path
from typing import List, Union, Optional

from comparator import Comparator, FileComparator, TextComparator, ValueComparator, NoComparator, SpecificComparator
from dodona import common as co, partial_output as po
from dodona.common import ExtendedMessage, Permission, supports_input_highlighting
from dodona.dodona import report_update
from runners.common import BaseRunner, ExecutionResult, get_runner
from tested import Config
from testplan import _get_stderr, _get_stdin, _get_stdout, Context, FunctionType, OutputChannelState, Plan, Testcase, \
    TestPlanError, BuiltinEvaluator, CustomEvaluator, \
    SpecificEvaluator, Builtin, OutputChannel, FileChannelState


def _get_or_default(seq, i, default):
    try:
        return seq[i]
    except IndexError:
        return default


def _get_comparator(output: Union[OutputChannel, OutputChannelState]) -> Optional[Comparator]:
    """
    Get the comparator for a given output.
    :param output: The output.
    :return: The comparator, or None if there is no comparator.
    """
    if output == OutputChannelState.none:
        return NoComparator()
    if output == OutputChannelState.ignored:
        return None
    assert isinstance(output, OutputChannel)
    evaluator = output.evaluator
    if isinstance(evaluator, BuiltinEvaluator):
        if evaluator.name == Builtin.text:
            return TextComparator(arguments=evaluator.options)
        elif evaluator.name == Builtin.file:
            return FileComparator(arguments=evaluator.options)
        elif evaluator.name == Builtin.value:
            return ValueComparator(arguments=evaluator.options)
        else:
            raise TestPlanError(f"Unknown built-in evaluator: {evaluator.name}")
    elif isinstance(evaluator, CustomEvaluator):
        raise NotImplementedError()
    elif isinstance(evaluator, SpecificEvaluator):
        return SpecificComparator()
    else:
        raise TestPlanError(f"Unknown evaluator type: {type(evaluator)}")


def _evaluate_channel(channel: str, expected, actual, evaluator: Comparator,
                      error_message: List[co.ExtendedMessage]):
    """
    Evaluate the output on a given channel. This function will output the appropriate messages
    to start and end a new test in Dodona.

    If expected is a :class:`OutputChannelState`, it will be handled according to the state;
    if the state is "ignored", this function will do nothing. When it is "none", the actual value
    should be empty.

    If errors is given, the test will end with a runtime error.

    :param channel: The name of the channel being evaluated. Will be displayed in Dodona.
    :param expected: The expected value, or None to skip evaluation.
    :param actual: The actual output. Ignored if expected is None.
    :param evaluator: The evaluator to use.
    :param error_message: The potential errors.
    """
    if expected == OutputChannelState.ignored:
        return  # Nothing to do.
    if expected == OutputChannelState.none:
        return  # Nothing to do
    report_update(po.StartTest(evaluator.get_readable_input(expected)))
    if error_message:
        for m in error_message:
            report_update(po.AppendMessage(m))
        status = po.Status.RUNTIME_ERROR
    else:
        success = evaluator.evaluate(expected, actual)
        status = po.Status.CORRECT if success else po.Status.WRONG
    if actual:
        actual = evaluator.get_readable_input(actual)
    else:
        actual = ""

    report_update(po.CloseTest(generated=actual,
                               status=po.StatusMessage(status),
                               data=po.TestData(channel)))


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


def _get_readable_input(case: Testcase, runner: BaseRunner) -> ExtendedMessage:
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

    runner: BaseRunner

    def __init__(self, config: Config):
        super().__init__(config)
        self.runner = get_runner(config)

    def _execute_test_plan(self, submission: str, test_plan: Plan):
        report_update(po.StartJudgment())

        # Generate test files.
        ids, ordered_files = self.runner.generate_code(submission, test_plan)

        # Compile the code if needed.
        # If a compilation error occurs, we stop the execution right now, and report the error.
        if self.runner.needs_compilation():
            compilation_result = self.runner.compile(ordered_files)
            if compilation_result.stdout:
                # Append compiler messages to the output.
                report_update(po.AppendMessage(co.ExtendedMessage(compilation_result.stdout, 'code')))
            if compilation_result.stderr:
                report_update(po.AppendMessage(co.ExtendedMessage(compilation_result.stderr, 'code')))
                report_update(po.CloseJudgment(accepted=False, status=po.StatusMessage(co.Status.COMPILATION_ERROR)))
                return

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
            # Get the evaluators
            try:
                stdout_evaluator = _get_comparator(testcase.output.stdout)
                stderr_evaluator = _get_comparator(testcase.output.stderr)
                file_evaluator = _get_comparator(testcase.output.file)
                result_evaluator = _get_comparator(testcase.output.result)
            except TestPlanError as e:
                report_update(po.AppendMessage(co.ExtendedMessage(str(e), 'text', co.Permission.STAFF)))
                break

            readable_input = _get_readable_input(testcase, self.runner)
            report_update(po.StartTestcase(readable_input))

            error_message: List[ExtendedMessage] = []

            # Evaluate the file channel.
            # We evaluate this channel early, since it is separate from the other channels.
            if testcase.output.file != FileChannelState.ignored:
                expected_file = testcase.output.file.expected_path
                actual_file = testcase.output.file.actual_path
                if expected_file and not path.exists(expected_file):
                    raise TestPlanError(f"Expected file {expected_file} not found.")
                if actual_file and not path.exists(actual_file):
                    error_message.append(co.ExtendedMessage(f"File {actual_file} does not exist.", 'text'))
                _evaluate_channel("file", expected_file, actual_file, file_evaluator, error_message)

            # The errors in the file channel have nothing to do with the other channels,
            # so reset them.
            error_message.clear()

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
                    error_message.clear()  # We assume this is the actual cause of the early termination.
                    error_message.append(co.ExtendedMessage(actual_stderr, 'code'))
            else:
                # Use it as a normal channel.
                _evaluate_channel("stderr", _get_stderr(testcase), actual_stderr, stderr_evaluator, error_message)

            # Evaluate the stdout channel.
            actual_stdout = stdout_[i] if i < len(stdout_) else None
            _evaluate_channel("stdout", _get_stdout(testcase), actual_stdout, stdout_evaluator, error_message)

            # Evaluate value channel
            try:
                value = values[i] if i < len(stdout_) else None
                actual_return = json.loads(value) if value else None
            except (ValueError, TypeError) as e:
                actual_return = None
                # Only if there are no errors yet.
                error_message.append(co.ExtendedMessage(traceback.format_exc(), 'code', Permission.STAFF))
                error_message.append(co.ExtendedMessage("Internal error while reading return value.", 'text'))
                raise e

            _evaluate_channel("return", testcase.output.result, actual_return, result_evaluator, error_message)

            report_update(po.CloseTestcase())

            # If this was an essential testcase with an error, stop testing now.
            if testcase.essential and error_message:
                break
