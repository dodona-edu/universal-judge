import logging
from pathlib import Path
from typing import Tuple, List

from .execution import ExecutionResult
from ..configs import Bundle
from ..dodona import *
from ..evaluators import Evaluator, get_evaluator
from ..languages.generator import get_readable_input
from ..testplan import Context, OutputChannel, EmptyChannel, \
    IgnoredChannel, ExitCodeOutputChannel, Testcase, ContextTestcase

_logger = logging.getLogger(__name__)


def _evaluate_channel(
        out: Union[UpdateCollector, IO],
        channel_name: str,
        is_timeout: bool,
        expected_output: OutputChannel,
        actual_result: Optional[str],
        evaluator: Evaluator) -> bool:
    """
    Evaluate the output on a given channel. This function will output the
    appropriate messages to start and end a new test in Dodona.

    If errors is given, the test will end with a runtime error. Note that if the
    channel output
    is None, the test will not be written to Dodona if everything is correct.

    If the actual result is None, this indicates that the test has not been
    executed. The same logic will be used to determine if the test should be
    reported or not.

    See https://github.com/dodona-edu/dodona/issues/1785, which will be used once
    implemented in Dodona itself. For now, a separate message it sent.

    :param out: The output file for the judge.
    :param channel_name: The name of the channel being evaluated. Will be
                         displayed in Dodona.
    :param expected_output: The output channel from the test case.
    :param actual_result: The actual output or None if the test did not run.
    :param evaluator: The evaluator to use.
    :return: True if successful, otherwise False.
    """
    evaluation_result = evaluator(expected_output, actual_result or "")
    status = evaluation_result.result

    # Override if timeout
    if is_timeout and status == Status.WRONG:
        status = Status.TIME_LIMIT_EXCEEDED

    # If the actual value is empty and the channel output is None or ignored,
    # don't report it.
    is_correct = status.enum == Status.CORRECT or actual_result is None
    has_no_result = actual_result is None or actual_result == ""
    has_no_expected = (expected_output == EmptyChannel.NONE
                       or expected_output == IgnoredChannel.IGNORED)
    is_exit_code = isinstance(expected_output, ExitCodeOutputChannel)
    if is_correct and ((has_no_result and has_no_expected) or is_exit_code):
        return True

    if actual_result is None:
        report_or_collect(out, StartTest(
            expected=evaluation_result.readable_expected,
            channel=channel_name
        ))
        report_or_collect(out, AppendMessage(
            message="Test niet uitgevoerd."
        ))
        report_or_collect(out, CloseTest(
            generated="",
            status=StatusMessage(
                enum=Status.TIME_LIMIT_EXCEEDED if is_timeout else Status.WRONG,
                human="Test niet uitgevoerd."
            )
        ))
        return False

    report_or_collect(out, StartTest(
        expected=evaluation_result.readable_expected,
        channel=channel_name
    ))

    # Report any messages we received.
    for message in evaluation_result.messages:
        report_or_collect(out, AppendMessage(message=message))

    # Close the test.
    report_or_collect(out, CloseTest(
        generated=evaluation_result.readable_actual,
        status=status
    ))

    return is_correct


def evaluate_results(bundle: Bundle,
                     context: Context,
                     exec_results: Optional[ExecutionResult],
                     compiler_results: Tuple[List[Message], Status],
                     context_dir: Path):
    # Begin by processing the context testcase.
    # Even if there is no main testcase, we can still proceed, since the defaults
    # should take care of this.
    testcase: ContextTestcase = context.context_testcase
    readable_input = get_readable_input(bundle, testcase)

    context_collector = UpdateCollector(StartTestcase(description=readable_input))

    # Handle the compiler output. If there is compiler output, there is no point in
    # checking additional testcases, so stop early.
    # Handle compiler results
    if compiler_results[1] != Status.CORRECT:
        # Report all compiler messages.
        for message in compiler_results[0]:
            context_collector.collect(AppendMessage(message=message))
        # Escalate the compiler status to every testcase.
        context_collector.collect(EscalateStatus(status=StatusMessage(
            enum=compiler_results[1]
        )))

        # Finish evaluation, since there is nothing we can do.
        context_collector.end(bundle.out, CloseTestcase(accepted=False))
        return

    # There must be execution if compilation succeeded.
    assert exec_results is not None

    # Split the basic output channels.
    # These channels should have one additional entry for the context testcase.
    stdout_ = exec_results.stdout.split(exec_results.separator)
    stderr_ = exec_results.stderr.split(exec_results.separator)
    exceptions = exec_results.exceptions.split(exec_results.separator)
    values = exec_results.results.split(exec_results.separator)

    # Proceed with evaluating the context testcase.
    # Get the evaluators. These take care of everything if there is no testcase.
    output = testcase.output
    stdout_evaluator = get_evaluator(bundle, context_dir, output.stdout)
    stderr_evaluator = get_evaluator(bundle, context_dir, output.stderr)
    file_evaluator = get_evaluator(bundle, context_dir, output.file)
    exception_evaluator = get_evaluator(bundle, context_dir, output.exception)

    # Collect some information for exit codes, which we evaluate as last.
    exit_output = output.exit_code
    exit_evaluator = get_evaluator(bundle, context_dir, exit_output)

    # Get the values produced by the execution. If there are no values, we use an
    # empty string at this time. We handle missing output later.
    # We use pop here, since we want to remove the values. That way, all result
    # arrays start at 0 for the normal testcases.
    actual_stderr = stderr_.pop(0) if stderr_ else ""
    actual_exception = exceptions.pop(0) if exceptions else ""
    actual_stdout = stdout_.pop(0) if stdout_ else ""
    # This is not actually evaluated, but for implementation reasons, the languages
    # still write a delimiter to it.
    _ = values.pop(0) if values else ""

    is_timout = exec_results.was_timeout

    # Actual do the evaluation.
    results = [
        _evaluate_channel(context_collector, "file", is_timout,
                          testcase.output.file, "", file_evaluator),
        _evaluate_channel(context_collector, "stderr", is_timout,
                          testcase.output.stderr, actual_stderr, stderr_evaluator),
        _evaluate_channel(context_collector, "exception", is_timout,
                          testcase.output.exception, actual_exception,
                          exception_evaluator),
        _evaluate_channel(context_collector, "stdout", is_timout,
                          testcase.output.stdout, actual_stdout, stdout_evaluator)
    ]

    # Check for missing values and stop if necessary.
    if not stdout_ or not stderr_ or not exceptions or not values:
        _logger.warning("Missing output in context testcase.")
        context_collector.collect(AppendMessage(
            "Ontbrekende uitvoerresultaten in Dodona. Er ging iets verkeerd!"
        ))
        # Recover stdout and stderr if present.
        if recovered := "\n".join(stdout_[1:]):
            context_collector.collect(AppendMessage(ExtendedMessage(
                description="Andere standaarduitvoer was:\n" + recovered,
                format="code"
            )))
        if recovered := "\n".join(stderr_[1:]):
            context_collector.collect(AppendMessage(ExtendedMessage(
                description="Andere standaardfout was:\n" + recovered,
                format="code"
            )))
        results.append(False)  # Ensure we stop.

    must_stop = False
    if not all(results):
        # As last item, we evaluate the exit code of the context.
        _evaluate_channel(context_collector, "exitcode", is_timout, exit_output,
                          str(exec_results.exit), exit_evaluator)
        must_stop = True

    # Done with the context testcase.
    context_collector.end(bundle.out, CloseTestcase())

    # Decide if we want to proceed.
    if must_stop:
        return  # Stop now.

    executed_testcases = 0

    # Begin processing the normal testcases.
    for i, testcase in enumerate(context.testcases):
        # Type hint for PyCharm.
        testcase: Testcase

        _logger.debug(f"Evaluating testcase {i}")

        readable_input = get_readable_input(bundle, testcase)
        report_update(bundle.out, StartTestcase(description=readable_input))

        # Get the evaluators
        output = testcase.output
        stdout_evaluator = get_evaluator(bundle, context_dir, output.stdout)
        stderr_evaluator = get_evaluator(bundle, context_dir, output.stderr)
        file_evaluator = get_evaluator(bundle, context_dir, output.file)
        value_evaluator = get_evaluator(bundle, context_dir, output.result)
        exception_evaluator = get_evaluator(bundle, context_dir, output.exception)

        # Get the values produced by the execution. If there are no values, we use
        # an empty string at this time. We handle missing output later.
        actual_stderr = stderr_[i] if i < len(stderr_) else ""
        actual_exception = exceptions[i] if i < len(exceptions) else ""
        actual_stdout = stdout_[i] if i < len(stdout_) else ""
        actual_value = values[i] if i < len(values) else ""

        results = [
            _evaluate_channel(bundle.out, "file", is_timout, testcase.output.file,
                              "", file_evaluator),
            _evaluate_channel(bundle.out, "stderr", is_timout,
                              testcase.output.stderr, actual_stderr,
                              stderr_evaluator),
            _evaluate_channel(bundle.out, "exception", is_timout,
                              testcase.output.exception, actual_exception,
                              exception_evaluator),
            _evaluate_channel(bundle.out, "stdout", is_timout,
                              testcase.output.stdout, actual_stdout,
                              stdout_evaluator),
            _evaluate_channel(bundle.out, "return", is_timout,
                              testcase.output.result, actual_value, value_evaluator)
        ]

        _logger.debug(f"IN TESTCASE {i}")
        _logger.debug(f"  stdout -> {stdout_}")
        _logger.debug(f"  stderr -> {stderr_}")
        _logger.debug(f"  values -> {values}")
        _logger.debug(f"  exceptions -> {exceptions}")

        # Check for missing values and stop if necessary.
        if (i >= len(stdout_)
                or i >= len(stderr_)
                or i >= len(values)
                or i >= len(exceptions)):
            _logger.warning(f"Missing output in testcase {i}")
            report_update(bundle.out, AppendMessage(
                "Ontbrekende uitvoerresultaten in Dodona. Er ging iets verkeerd!"
            ))
            # Recover stdout and stderr if present.
            if recovered := "\n".join(stdout_[i:]):
                context_collector.collect(AppendMessage(ExtendedMessage(
                    description="Andere standaarduitvoer was:\n" + recovered,
                    format="code"
                )))
            if recovered := "\n".join(stderr_[i:]):
                context_collector.collect(AppendMessage(ExtendedMessage(
                    description="Andere standaardfout was:\n" + recovered,
                    format="code"
                )))
            super_stop = True
        else:
            super_stop = False

        # Decide if we want to proceed.
        if (testcase.essential and not all(results)) or super_stop:
            # As last item, we evaluate the exit code of the context.
            _evaluate_channel(bundle.out, "exitcode", is_timout, exit_output,
                              str(exec_results.exit), exit_evaluator)
            must_stop = True

        report_update(bundle.out, CloseTestcase())

        executed_testcases = i

        if must_stop:
            _logger.debug("Stopping evaluation, since testcase is essential.")
            break  # Stop evaluation now.

    # TODO: merge all three evaluations: context, testcases and not handled, since
    #  they are quite similar but also not the same.

    start = executed_testcases + 1

    for i, testcase in enumerate(context.testcases[start:], start):
        # Type hint for PyCharm.
        testcase: Testcase

        _logger.debug(f"Evaluating not executed testcase {i}")

        readable_input = get_readable_input(bundle, testcase)
        report_update(bundle.out, StartTestcase(description=readable_input))

        # Get the evaluators
        output = testcase.output
        stdout_evaluator = get_evaluator(bundle, context_dir, output.stdout)
        stderr_evaluator = get_evaluator(bundle, context_dir, output.stderr)
        file_evaluator = get_evaluator(bundle, context_dir, output.file)
        value_evaluator = get_evaluator(bundle, context_dir, output.result)
        exception_evaluator = get_evaluator(bundle, context_dir, output.exception)

        # Get the values produced by the execution. If there are no values, we use
        # an empty string at this time. We handle missing output later.
        actual_stderr = stderr_[i] if i < len(stderr_) else None
        actual_exception = exceptions[i] if i < len(exceptions) else None
        actual_stdout = stdout_[i] if i < len(stdout_) else None
        actual_value = values[i] if i < len(values) else None

        _evaluate_channel(bundle.out, "file", is_timout, testcase.output.file, "",
                          file_evaluator)
        _evaluate_channel(bundle.out, "stderr", is_timout, testcase.output.stderr,
                          actual_stderr, stderr_evaluator)
        _evaluate_channel(bundle.out, "exception", is_timout,
                          testcase.output.exception, actual_exception,
                          exception_evaluator)
        _evaluate_channel(bundle.out, "stdout", is_timout, testcase.output.stdout,
                          actual_stdout, stdout_evaluator)
        _evaluate_channel(bundle.out, "return", is_timout, testcase.output.result,
                          actual_value, value_evaluator)

        report_update(bundle.out, CloseTestcase())
