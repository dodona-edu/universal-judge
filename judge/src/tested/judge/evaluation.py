import logging
import time
from pathlib import Path
from typing import Tuple, List

from .collector import OutputManager, TestcaseCollector
from .execution import ExecutionResult
from ..configs import Bundle
from ..dodona import *
from ..dodona import StartTestcase, CloseTestcase
from ..evaluators import Evaluator, get_evaluator
from ..languages.generator import get_readable_input, attempt_readable_input
from ..testplan import Context, OutputChannel, EmptyChannel, \
    IgnoredChannel, ExitCodeOutputChannel, Testcase, ContextTestcase

_logger = logging.getLogger(__name__)


def _evaluate_channel(
        out: TestcaseCollector,
        channel_name: str,
        expected_output: OutputChannel,
        actual_result: Optional[str],
        evaluator: Evaluator,
        max_time: Optional[float] = None) -> bool:
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
    :param max_time: The max amount of time.

    :return: True if successful, otherwise False.
    """
    evaluation_result = evaluator(
        expected_output,
        actual_result if actual_result else "",
        Status.WRONG if not max_time or max_time > 0 else
        Status.TIME_LIMIT_EXCEEDED,
        max_time
    )
    status = evaluation_result.result

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
        out.add(StartTest(
            expected=evaluation_result.readable_expected,
            channel=channel_name
        ))
        out.add(AppendMessage(
            message="Test niet uitgevoerd."
        ))
        out.add(CloseTest(
            generated="",
            status=StatusMessage(
                enum=Status.WRONG,
                human="Test niet uitgevoerd."
            )
        ))
        return False

    out.add(StartTest(
        expected=evaluation_result.readable_expected,
        channel=channel_name
    ))

    # Report any messages we received.
    for message in evaluation_result.messages:
        out.add(AppendMessage(message=message))

    # Close the test.
    out.add(CloseTest(
        generated=evaluation_result.readable_actual,
        status=status
    ))

    return is_correct


def evaluate_results(bundle: Bundle,
                     context: Context,
                     exec_results: Optional[ExecutionResult],
                     compiler_results: Tuple[List[Message], Status],
                     context_dir: Path,
                     collector: OutputManager,
                     max_time: float) -> Optional[Status]:
    # Begin by processing the context testcase.
    # Even if there is no main testcase, we can still proceed, since the defaults
    # should take care of this.
    start = time.perf_counter()

    # Handle the compiler output. If there is compiler output, there is no point in
    # checking additional testcases, so stop early.
    # Handle compiler results
    if compiler_results[1] != Status.CORRECT:
        readable_input = attempt_readable_input(bundle, context)
        collector.add(StartTestcase(description=readable_input))
        # Report all compiler messages.
        for message in compiler_results[0]:
            collector.add(AppendMessage(message=message))
        # Escalate the compiler status to every testcase.
        collector.add(EscalateStatus(status=StatusMessage(
            enum=compiler_results[1]
        )))

        # Finish evaluation, since there is nothing we can do.
        collector.add(CloseTestcase(accepted=False))
        return None

    testcase: ContextTestcase = context.context_testcase
    readable_input = get_readable_input(bundle, testcase)

    context_collector = TestcaseCollector(StartTestcase(description=readable_input))

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

    remaining = max_time - (time.perf_counter() - start)
    # Actual do the evaluation.
    results = [_evaluate_channel(
        context_collector, "file", output.file, "", file_evaluator, remaining
    )]
    remaining = max_time - (time.perf_counter() - start)
    results.append(_evaluate_channel(
        context_collector, "stderr", output.stderr, actual_stderr, stderr_evaluator,
        remaining
    ))
    remaining = max_time - (time.perf_counter() - start)
    results.append(_evaluate_channel(
        context_collector, "exception", output.exception, actual_exception,
        exception_evaluator, remaining
    ))
    remaining = max_time - (time.perf_counter() - start)
    results.append(_evaluate_channel(
        context_collector, "stdout", output.stdout, actual_stdout, stdout_evaluator,
        remaining
    ))

    # Check for missing values and stop if necessary.
    if not stdout_ or not stderr_ or not exceptions or not values:
        _logger.warning("Missing output in context testcase.")
        context_collector.add(AppendMessage(
            "Ontbrekende uitvoerresultaten in Dodona. Er ging iets verkeerd!"
        ))
        context_collector.add(EscalateStatus(status=StatusMessage(
            enum=Status.WRONG,
            human="Ontbrekende uitvoer."
        )))
        # Recover stdout and stderr if present.
        if recovered := "\n".join(stdout_[1:]):
            context_collector.add(AppendMessage(ExtendedMessage(
                description="Andere standaarduitvoer was:\n" + recovered,
                format="code"
            )))
        if recovered := "\n".join(stderr_[1:]):
            context_collector.add(AppendMessage(ExtendedMessage(
                description="Andere standaardfout was:\n" + recovered,
                format="code"
            )))
        results.append(False)  # Ensure we stop.

    must_stop = False
    if not all(results):
        remaining = max_time - (time.perf_counter() - start)
        # As last item, we evaluate the exit code of the context.
        _evaluate_channel(context_collector, "exitcode", exit_output,
                          str(exec_results.exit), exit_evaluator, remaining)
        must_stop = True

    # Done with the context testcase.
    context_collector.to_manager(collector, CloseTestcase())

    # Decide if we want to proceed.
    if must_stop:
        if exec_results.timeout:
            return Status.TIME_LIMIT_EXCEEDED
        return None  # Stop now.

    # Begin processing the normal testcases.
    for i, testcase in enumerate(context.testcases):
        # Type hint for PyCharm.
        testcase: Testcase

        _logger.debug(f"Evaluating testcase {i}")

        readable_input = get_readable_input(bundle, testcase)
        t_col = TestcaseCollector(StartTestcase(description=readable_input))

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

        remaining = max_time - (time.perf_counter() - start)
        results = [
            _evaluate_channel(t_col, "file", output.file, "", file_evaluator,
                              remaining)
        ]
        remaining = max_time - (time.perf_counter() - start)
        results.append(_evaluate_channel(
            t_col, "stderr", output.stderr, actual_stderr, stderr_evaluator,
            remaining
        )),
        remaining = max_time - (time.perf_counter() - start)
        results.append(_evaluate_channel(
            t_col, "exception", output.exception, actual_exception,
            exception_evaluator, remaining
        )),
        remaining = max_time - (time.perf_counter() - start)
        results.append(_evaluate_channel(
            t_col, "stdout", output.stdout, actual_stdout, stdout_evaluator,
            remaining
        )),
        remaining = max_time - (time.perf_counter() - start)
        results.append(_evaluate_channel(
            t_col, "return", output.result, actual_value, value_evaluator,
            remaining
        ))

        # Check for missing values and stop if necessary.
        if (i >= len(stdout_)
                or i >= len(stderr_)
                or i >= len(values)
                or i >= len(exceptions)):
            _logger.warning(f"Missing output in testcase {i}")
            t_col.add(AppendMessage(
                "Ontbrekende uitvoerresultaten in Dodona. Er ging iets verkeerd!"
            ))
            # Recover stdout and stderr if present.
            if recovered := "\n".join(stdout_[i:]):
                t_col.add(AppendMessage(ExtendedMessage(
                    description="Andere standaarduitvoer was:\n" + recovered,
                    format="code"
                )))
            if recovered := "\n".join(stderr_[i:]):
                t_col.add(AppendMessage(ExtendedMessage(
                    description="Andere standaardfout was:\n" + recovered,
                    format="code"
                )))
            super_stop = True
        else:
            super_stop = False

        # Decide if we want to proceed.
        if ((testcase.essential and not all(results))
                or super_stop
                or i == len(context.testcases) - 1):
            remaining = max_time - (time.perf_counter() - start)
            # As last item, we evaluate the exit code of the context.
            _evaluate_channel(t_col, "exitcode", exit_output,
                              str(exec_results.exit), exit_evaluator, remaining)
            must_stop = True

        t_col.to_manager(collector, CloseTestcase())

        if must_stop:
            if exec_results.timeout:
                return Status.TIME_LIMIT_EXCEEDED
            return None  # Stop evaluation now.


def prepare_evaluation(bundle: Bundle, collector: OutputManager):
    # This is very similar to the normal evaluation, see the docs there.
    collector.prepare_judgment(StartJudgment())
    for i, tab in enumerate(bundle.plan.tabs):
        collector.prepare_tab(StartTab(title=tab.name), i)
        for j, context in enumerate(tab.contexts):
            collector.prepare_context(StartContext(description=context.description),
                                      i, j)
            context_updates = []
            # Begin with the context testcase.
            c_dir = Path(
                bundle.config.workdir,
                bundle.lang_config.context_name(
                    tab_number=i,
                    context_number=j
                )
            )
            readable_input = get_readable_input(bundle, context.context_testcase)
            c_col = TestcaseCollector(StartTestcase(description=readable_input))

            # Get the evaluators for the tests.
            output = context.context_testcase.output
            stdout_eval = get_evaluator(bundle, c_dir, output.stdout)
            stderr_eval = get_evaluator(bundle, c_dir, output.stderr)
            file_eval = get_evaluator(bundle, c_dir, output.file)
            exc_eval = get_evaluator(bundle, c_dir, output.exception)

            # Get exit code stuff, but don't evaluate yet.
            exit_output = output.exit_code
            exit_evaluator = get_evaluator(bundle, c_dir, exit_output)

            # Do the tests.
            _evaluate_channel(c_col, "file", output.file, None, file_eval),
            _evaluate_channel(c_col, "stderr", output.stderr, None, stderr_eval),
            _evaluate_channel(c_col, "exception", output.exception, None, exc_eval),
            _evaluate_channel(c_col, "stdout", output.stdout, None, stdout_eval)
            context_updates.extend(c_col.as_list(CloseTestcase()))

            # Begin normal testcases.
            for t, testcase in enumerate(context.testcases, 1):
                testcase: Testcase
                readable_input = get_readable_input(bundle, testcase)
                t_col = TestcaseCollector(StartTestcase(description=readable_input))

                # Get the evaluators for the tests.
                output = testcase.output
                stdout_eval = get_evaluator(bundle, c_dir, output.stdout)
                stderr_eval = get_evaluator(bundle, c_dir, output.stderr)
                file_eval = get_evaluator(bundle, c_dir, output.file)
                value_evaluator = get_evaluator(bundle, c_dir, output.result)
                exc_eval = get_evaluator(bundle, c_dir, output.exception)

                # Do the tests.
                _evaluate_channel(t_col, "file", output.file, None, file_eval),
                _evaluate_channel(t_col, "stderr",
                                  output.stderr, None, stderr_eval),
                _evaluate_channel(t_col, "exception",
                                  output.exception, None, exc_eval),
                _evaluate_channel(t_col, "stdout",
                                  output.stdout, None, stdout_eval),
                _evaluate_channel(t_col, "return",
                                  output.result, None, value_evaluator)

                # If last testcase, do exit code.
                if t == len(context.testcases):
                    _evaluate_channel(t_col, "exitcode",
                                      exit_output, None, exit_evaluator)

                context_updates.extend(t_col.as_list(CloseTestcase()))

            collector.prepare_context(context_updates, i, j)
            collector.prepare_context(CloseContext(), i, j)

        collector.prepare_tab(CloseTab(), i)
    collector.prepare_judgment(CloseJudgment())
