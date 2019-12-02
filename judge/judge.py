from pprint import pprint

from dodona import *
from evaluators import get_evaluator, Evaluator
from runners.runner import BaseRunner, ExecutionResult, get_runner
from tested import Config
from testplan import *


def _get_or_default(seq, i, default):
    try:
        return seq[i]
    except IndexError:
        return default


def _evaluate_channel(channel_name: str,
                      expected_output: Union[OutputChannel, AnyChannelState],
                      actual_result: Optional[str],
                      evaluator: Evaluator) -> bool:
    """
    Evaluate the output on a given channel. This function will output the appropriate messages
    to start and end a new test in Dodona.

    If errors is given, the test will end with a runtime error. Note that if the expected output
    is None, the test will not be written to Dodona if everything is correct.

    :param channel_name: The name of the channel being evaluated. Will be displayed in Dodona.
    :param expected_output: The output channel from the test case.
    :param actual_result: The actual output. Can be None, depending on the evaluator.
    :param evaluator: The evaluator to use.
    :return: True if successful, otherwise False.
    """
    evaluation_result = evaluator.evaluate(expected_output, actual_result)
    status = evaluation_result.result

    report_update(StartTest(expected=evaluation_result.readable_expected))

    # Report any messages we received.
    for message in evaluation_result.messages:
        report_update(AppendMessage(message=message))

    # Close the test.
    report_update(CloseTest(generated=evaluation_result.readable_actual,
                            status=status,
                            data=TestData(channel=channel_name)))

    return status == Status.CORRECT


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


class GeneratorJudge(Judge):
    runner: BaseRunner

    def __init__(self, config: Config):
        super().__init__(config)
        self.runner = get_runner(config)

    def _execute_test_plan(self, submission: str, test_plan: Plan):
        report_update(StartJudgment())

        # Generate test files.
        ids, ordered_files = self.runner.generate_code(submission, test_plan)

        # Compile the code if needed.
        # If a compilation error occurs, we stop the execution right now, and report the error.
        if self.runner.needs_compilation():
            compilation_result = self.runner.compile(ordered_files)
            if compilation_result.stdout:
                # Append compiler messages to the output.
                report_update(AppendMessage(message=ExtendedMessage(
                    description=compilation_result.stdout,
                    format='code'
                )))
            if compilation_result.stderr:
                report_update(AppendMessage(message=ExtendedMessage(
                    description=compilation_result.stderr,
                    format='code'
                )))
                report_update(CloseJudgment(status=StatusMessage(enum=Status.COMPILATION_ERROR)))
                return

        for tab in test_plan.tabs:
            report_update(StartTab(title=tab.name))
            for id_, context in zip(ids, tab.contexts):
                report_update(StartContext(description=context.description))
                try:
                    result = self.runner.execute(id_, context)
                except TestPlanError as e:
                    report_update(AppendMessage(message=ExtendedMessage(
                        description=str(e),
                        format='text',
                        permission=Permission.STAFF
                    )))
                    report_update(CloseJudgment(status=StatusMessage(enum=Status.INTERNAL_ERROR)))
                    continue
                self._process_results(context, result)
                report_update(CloseContext())
            report_update(CloseTab())
        report_update(CloseJudgment())

    def _process_results(self, context: Context, results: ExecutionResult):
        # Process output
        stdout_ = results.stdout.split(results.separator)
        stderr_ = results.stderr.split(results.separator)
        values = results.results.split(results.separator)
        exceptions = results.exceptions.split(results.separator)

        # There might be less output than testcase, which is an error. However, we process the
        # output we have, to ensure we send as much feedback as possible to the user.
        for i, testcase in enumerate(context.all_testcases()):
            # Get the evaluators
            try:
                stdout_evaluator = get_evaluator(self.config, testcase.stdout)
                stderr_evaluator = get_evaluator(self.config, testcase.stderr)
                file_evaluator = get_evaluator(self.config, testcase.file)
                value_evaluator = get_evaluator(self.config, testcase.result)
                exception_evaluator = get_evaluator(self.config, testcase.exception)
            except TestPlanError as e:
                report_update(AppendMessage(message=ExtendedMessage(
                    description=str(e),
                    format='text',
                    permission=Permission.STAFF
                )))
                break

            readable_input = self.runner.get_readable_input(context, testcase)
            report_update(StartTestcase(description=readable_input))

            # Evaluate the file channel.
            results = [_evaluate_channel("file", testcase.file, None, file_evaluator)]

            # Evaluate the stderr channel
            actual_stderr = stderr_[i] if i < len(stderr_) else None
            results.append(_evaluate_channel("stderr", testcase.stderr, actual_stderr, stderr_evaluator))

            actual_exception = exceptions[i] if i < len(exceptions) else None
            results.append(_evaluate_channel("exception", testcase.exception, actual_exception, exception_evaluator))

            actual_stdout = stdout_[i] if i < len(stdout_) else None
            results.append(_evaluate_channel("stdout", testcase.stdout, actual_stdout, stdout_evaluator))

            actual_value = values[i] if i < len(values) else None
            results.append(_evaluate_channel("return", testcase.result, actual_value, value_evaluator))

            # Check if there is early termination.
            if i >= len(stdout_) or i >= len(stderr_) or i >= len(values) or i >= len(exceptions):
                report_update(AppendMessage(message=ExtendedMessage(
                    description="Tests were terminated early.", format='text'
                )))

            report_update(CloseTestcase())

            # If this was an essential testcase with an error, stop testing now.
            if testcase.essential and not all(results):
                break
