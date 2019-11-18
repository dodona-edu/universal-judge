from dodona import *
from evaluators import get_evaluator, Evaluator
from runners.common import BaseRunner, ExecutionResult, get_runner
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
                      evaluator: Evaluator,
                      error_message: List[ExtendedMessage]):
    """
    Evaluate the output on a given channel. This function will output the appropriate messages
    to start and end a new test in Dodona.

    If errors is given, the test will end with a runtime error.

    :param channel_name: The name of the channel being evaluated. Will be displayed in Dodona.
    :param expected_output: The output channel from the test case.
    :param actual_result: The actual output. Can be None, depending on the evaluator.
    :param evaluator: The evaluator to use.
    :param error_message: The potential errors.
    """
    evaluation_result = evaluator.evaluate(expected_output, actual_result)
    status = evaluation_result.result

    # Start the test.
    report_update(StartTest(expected=evaluation_result.readable_expected))

    # Report any errors we got.
    if error_message:
        for m in error_message:
            report_update(AppendMessage(message=m))
        status = StatusMessage(enum=Status.RUNTIME_ERROR)

    # Report any other messages we received.
    for message in evaluation_result.messages:
        report_update(AppendMessage(message=message))

    # Close the test.
    report_update(CloseTest(generated=evaluation_result.readable_actual,
                            status=status,
                            data=TestData(channel=channel_name)))


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


def get_readable_input(case: Testcase, runner: BaseRunner) -> ExtendedMessage:
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
    format_ = 'text'  # By default, we use text as input.
    if case.description:
        text = case.description
    elif case.function and (case.function.type != FunctionType.MAIN or runner.needs_main()):
        text = runner.function_call(case.function)
        # TODO: should we check if highlighting is supported or not?
        format_ = runner.config.programming_language
    elif case.stdin != NoneChannelState.NONE:
        assert isinstance(case.stdin, ChannelData)
        text = case.stdin.get_data_as_string()
    else:
        # If there is no stdin, but there is a main function but we end up here, this means the
        # language does not use main functions. In that case, we use a placeholder.
        if case.function and case.function.type == FunctionType.MAIN:
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

        # There might be less output than testcase, which is an error. However, we process the
        # output we have, to ensure we send as much feedback as possible to the user.
        for i, testcase in enumerate(context.all_testcases()):
            # Get the evaluators
            try:
                stdout_evaluator = get_evaluator(testcase.stdout)
                stderr_evaluator = get_evaluator(testcase.stderr)
                file_evaluator = get_evaluator(testcase.file)
                result_evaluator = get_evaluator(testcase.result)
            except TestPlanError as e:
                report_update(AppendMessage(message=ExtendedMessage(
                    description=str(e),
                    format='text',
                    permission=Permission.STAFF
                )))
                break

            readable_input = get_readable_input(testcase, self.runner)
            report_update(StartTestcase(description=readable_input))

            error_message: List[ExtendedMessage] = []

            # Evaluate the file channel.
            # We evaluate this channel early, since it is separate from the other channels.
            _evaluate_channel("file", testcase.file, None, file_evaluator, error_message)

            # The errors in the file channel have nothing to do with the other channels,
            # so reset them.
            error_message.clear()

            # Check if there is early termination.
            if i >= len(stdout_) or i >= len(stderr_) or i >= len(values):
                assert i >= len(stdout_) and i >= len(stderr_) and i >= len(values)
                error_message.append(ExtendedMessage(description="Tests were terminated early.", format='text'))

            # Evaluate the error channel.
            # If we expect no errors, we produce an error message, which is used in subsequent checks.
            # However, if we do expected something on this channel, we treat it as a normal channel.
            actual_stderr = stderr_[i] if i < len(stderr_) else None
            if testcase.stderr == NoneChannelState.NONE:
                # Use it as an error message, if it exists.
                if actual_stderr:
                    error_message.clear()  # We assume this is the actual cause of the early termination.
                    error_message.append(ExtendedMessage(description=actual_stderr, format='code'))
            else:
                # Use it as a normal channel.
                _evaluate_channel("stderr", testcase.stdout, actual_stderr, stderr_evaluator, error_message)

            # Evaluate the stdout channel.
            actual_stdout = stdout_[i] if i < len(stdout_) else None
            _evaluate_channel("stdout", testcase.stdout, actual_stdout, stdout_evaluator, error_message)

            # Evaluate value channel
            actual_value = values[i] if i < len(values) else None
            _evaluate_channel("return", testcase.result, actual_value, result_evaluator, error_message)

            report_update(CloseTestcase())

            # If this was an essential testcase with an error, stop testing now.
            if testcase.essential and error_message:
                break
