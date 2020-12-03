import html
import logging
from pathlib import Path
from typing import Tuple, List

from .collector import OutputManager, TestcaseCollector
from .execution import ExecutionResult
from ..configs import Bundle
from ..dodona import *
from ..dodona import StartTestcase, CloseTestcase
from ..evaluators import get_evaluator
from ..languages.generator import get_readable_input, attempt_readable_input, \
    convert_statement
from ..testplan import Context, OutputChannel, IgnoredChannel, \
    ExitCodeOutputChannel, Testcase, ContextTestcase, TextOutput, \
    FileOutput, ValueOutput, TextOutputChannel, SpecialOutputChannel, \
    FileOutputChannel, ExceptionOutput, ExceptionOutputChannel, ValueOutputChannel
from ..utils import get_args, safe_del, safe_get

_logger = logging.getLogger(__name__)


class Channel(str, Enum):
    """
    The different output channels.
    """
    FILE = "file"
    EXCEPTION = "exception"
    STDOUT = "stdout"
    STDERR = "stderr"
    EXIT = "exit code"
    RETURN = "return"


def _evaluate_channel(
        bundle: Bundle,
        context_directory: Path,
        out: TestcaseCollector,
        channel: Channel,
        output: OutputChannel,
        actual: Optional[str]) -> Optional[bool]:
    """
    Evaluate the output on a given channel. This function will output the
    appropriate messages to start and end a new test in Dodona.

    Depending on the channel, the result might not be outputted to Dodona. For
    example, if an exit code is 0 and also expected to be 0, it will not be shown,
    as it is assumed to be an implicit test. There is currently no way to override
    this.

    If the actual result is None, this means something went wrong while executing
    the code (like a premature exit call). A message will be added and the channel
    will be shown, regardless of value.

    :param out: The output file for the judge.
    :param channel: The name of the channel being evaluated. Will be
                         displayed in Dodona.
    :param output: The output channel from the test case.
    :param actual: The actual output or None if the result is missing.
    :param bundle: The configuration bundle.
    :param context_directory: The directory in which the execution took place.

    :return: True if successful, otherwise False.
    """
    evaluator = get_evaluator(bundle, context_directory, output)
    # Run the evaluator.
    evaluation_result = evaluator(
        output,
        actual if actual else ""
    )
    status = evaluation_result.result

    # Decide if we should show this channel or not.
    is_correct = status.enum == Status.CORRECT

    if not should_show(output, channel) and is_correct:
        return True

    out.add(StartTest(
        expected=evaluation_result.readable_expected,
        channel=channel
    ))

    # Report any messages we received.
    for message in evaluation_result.messages:
        out.add(AppendMessage(message=message))

    # Report missing output
    if actual is None:
        out.add(AppendMessage(
            message="De beoordeling is vroegtijdig gestopt."
        ))

    # Close the test.
    out.add(CloseTest(
        generated=evaluation_result.readable_actual,
        status=status
    ))

    return is_correct


def evaluate_results(bundle: Bundle, context: Context,
                     exec_results: Optional[ExecutionResult],
                     compiler_results: Tuple[List[Message], Status],
                     context_dir: Path,
                     collector: OutputManager) -> Optional[Status]:
    # Begin by processing the context testcase.
    # Even if there is no main testcase, we can still proceed, since the defaults
    # should take care of this.

    # Add file links
    if context.link_files:
        dict_links = dict((link_file.name, dataclasses.asdict(link_file))
                          for link_file in context.link_files)
        dict_json = json.dumps(dict_links)
        link_list = '\n'.join(
            '<li><a href="#" class="file-link">'
            f'{html.escape(link_file.name)}</a></li>'
            for link_file in context.link_files
        )
        description = f"""
<div class="contains-file" data-files='{dict_json}'>
    <ul>
        {link_list}
    </ul>
</div>
        """.strip()
        message = ExtendedMessage(description=description, format="html")
        collector.add(AppendMessage(message=message))

    # Handle the compiler output. If there is compiler output, there is no
    # point in
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

    # The first item should always be empty, since the separator must be printed
    # before the testplan runs. We remove the first item; for stdout and stderr
    # we only remove the first item if it is indeed empty. This is to keep error
    # messages present for debugging.

    deletions = (
        safe_del(stdout_, 0, lambda e: e == ''),
        safe_del(stderr_, 0, lambda e: e == ''),
        safe_del(exceptions, 0, lambda e: e == ''),
        safe_del(values, 0, lambda e: e == '')
    )

    could_delete = all(deletions)

    # Add a message indicating there were missing values.
    missing_values = []
    if not could_delete:
        _logger.warning("Missing output in context testcase.")
        missing_values.append(AppendMessage(
            "De beoordeling is vroegtijdig gestopt."
        ))
        missing_values.append(EscalateStatus(status=StatusMessage(
            enum=Status.WRONG,
            human="Ontbrekende uitvoer."
        )))
        # Recover stdout and stderr if present.
        if recovered := "\n".join(stdout_):
            missing_values.append(AppendMessage(ExtendedMessage(
                description="Standaarduitvoer was:\n" + recovered,
                format="code"
            )))
        if recovered := "\n".join(stderr_):
            missing_values.append(AppendMessage(ExtendedMessage(
                description="Standaardfout was:\n" + recovered,
                format="code"
            )))

    # Proceed with evaluating the context testcase.
    # Get the evaluators. These take care of everything if there is no testcase.
    output = testcase.output

    # Collect some information for exit codes, which we evaluate as last.
    exit_output = output.exit_code

    actual_stderr = safe_get(stderr_, 0)
    actual_exception = safe_get(exceptions, 0)
    actual_stdout = safe_get(stdout_, 0)

    # Actual do the evaluation.
    # This will also print a message if there are missing values for this testcase
    # (but not if there are missing values before the testcase, we handle this
    # further down)
    results = [
        could_delete,
        _evaluate_channel(
            bundle, context_dir, context_collector, Channel.FILE, output.file, ""
        ),
        _evaluate_channel(
            bundle, context_dir, context_collector, Channel.STDERR, output.stderr,
            actual_stderr
        ),
        _evaluate_channel(
            bundle, context_dir, context_collector, Channel.EXCEPTION,
            output.exception, actual_exception
        ),
        _evaluate_channel(
            bundle, context_dir, context_collector, Channel.STDOUT, output.stdout,
            actual_stdout
        )
    ]

    # Check if we should stop now or proceed to the next testcase.
    # This is needed when there is no main testcase and the first normal testcase
    # failed.
    mem_or_time = exec_results.timeout or exec_results.memory
    has_main = testcase.input.main_call
    is_correct = all(results)
    should_stop = has_main and (not is_correct or mem_or_time)

    if has_main:
        for u in missing_values:
            context_collector.add(u)

    # If we must stop due to errors are there no further testcases, evaluate the
    # exit channel.
    if should_stop or not context.testcases:
        _evaluate_channel(
            bundle, context_dir, context_collector, Channel.EXIT, exit_output,
            str(exec_results.exit)
        )

    # Done with the context testcase.
    context_collector.to_manager(collector, CloseTestcase())

    # Decide if we want to proceed.
    if should_stop:
        if exec_results.timeout:
            return Status.TIME_LIMIT_EXCEEDED
        if exec_results.memory:
            return Status.MEMORY_LIMIT_EXCEEDED
        return None  # Stop now.

    # Begin processing the normal testcases.
    for i, testcase in enumerate(context.testcases, 1):
        # Type hint for PyCharm.
        testcase: Testcase

        _logger.debug(f"Evaluating testcase {i}")

        readable_input = get_readable_input(bundle, testcase)
        t_col = TestcaseCollector(StartTestcase(description=readable_input))

        # Get the evaluators
        output = testcase.output

        # Get the values produced by the execution. If there are no values, we use
        # an empty string at this time. We handle missing output later.
        actual_stderr = safe_get(stderr_, i)
        actual_exception = safe_get(exceptions, i)
        actual_stdout = safe_get(stdout_, i)
        actual_value = safe_get(values, i)

        results = [
            _evaluate_channel(
                bundle, context_dir, t_col, Channel.FILE, output.file, ""
            ),
            _evaluate_channel(
                bundle, context_dir, t_col, Channel.STDERR, output.stderr,
                actual_stderr
            ),
            _evaluate_channel(
                bundle, context_dir, t_col, Channel.EXCEPTION, output.exception,
                actual_exception
            ),
            _evaluate_channel(
                bundle, context_dir, t_col, Channel.STDOUT, output.stdout,
                actual_stdout
            ),
            _evaluate_channel(
                bundle, context_dir, t_col, Channel.RETURN, output.result,
                actual_value
            )
        ]

        should_stop = False
        # If there are missing values, stop.
        if None in (actual_stderr, actual_exception, actual_stdout, actual_value):
            should_stop = True

        # If there was a memory or time error, stop.
        if exec_results.timeout or exec_results.memory:
            should_stop = True

        # If this testcase is essential and there were errors, stop.
        if testcase.essential and not all(results):
            should_stop = True

        # If we will stop or this is the last one, do the exit channel.
        if should_stop or i == len(context.testcases):
            _evaluate_channel(
                bundle, context_dir, t_col, Channel.EXIT, exit_output,
                str(exec_results.exit)
            )

        # Add messages if there was no main file.
        if missing_values and not has_main:
            for u in missing_values:
                t_col.add(u)
            should_stop = True

        # Stop with this testcase.
        t_col.to_manager(collector, CloseTestcase())

        if should_stop:
            if exec_results.timeout:
                return Status.TIME_LIMIT_EXCEEDED
            if exec_results.memory:
                return Status.MEMORY_LIMIT_EXCEEDED
            return None  # Stop evaluation now.


def should_show(test: OutputChannel, channel: Channel) -> bool:
    """
    Determine if the channel should be shown, without accounting for the actual
    value. This function answers the question: "Assuming the actual value is
    correct, should we show this output channel?".

    :param test: The output for the channel from the testplan.
    :param channel: The channel.

    :return: True if the channel should be shown, false otherwise.
    """
    if channel == Channel.EXIT:
        assert isinstance(test, get_args(ExitCodeOutputChannel))
        return test.value != 0
    elif channel in (Channel.STDOUT, Channel.STDERR):
        assert isinstance(test, get_args(TextOutput))
        # We don't show the channel if the output is nothing or ignored.
        return not isinstance(test, get_args(SpecialOutputChannel))
    elif channel == Channel.FILE:
        assert isinstance(test, get_args(FileOutput))
        # We don't show the channel if we ignore the channel.
        return not isinstance(test, IgnoredChannel)
    elif channel == Channel.RETURN:
        assert isinstance(test, get_args(ValueOutput))
        # We don't show the channel if we ignore it or expect no result.
        return not isinstance(test, get_args(SpecialOutputChannel))
    elif channel == Channel.EXCEPTION:
        assert isinstance(test, get_args(ExceptionOutput))
        return not isinstance(test, get_args(SpecialOutputChannel))
    else:
        raise AssertionError(f"Unknown channel {channel}")


def guess_expected_value(bundle: Bundle, test: OutputChannel) -> str:
    """
    Try and get the expected value for a output channel. In some cases, such as
    a programmed or language specific evaluator, there is will be no expected value
    available in the testplan. In that case, we use an empty string.

    :param bundle: Configuration bundle.
    :param test: The output channel.

    :return: A best effort attempt of the expected value.
    """
    if isinstance(test, get_args(SpecialOutputChannel)):
        return ""
    elif isinstance(test, TextOutputChannel):
        return test.get_data_as_string(bundle.config.resources)
    elif isinstance(test, FileOutputChannel):
        return test.get_data_as_string(bundle.config.resources)
    elif isinstance(test, ExceptionOutputChannel):
        return test.exception.message if test.exception else "Dynamisch"
    elif isinstance(test, ValueOutputChannel):
        return convert_statement(bundle, test.value) if test.value else "Dynamisch"
    elif isinstance(test, ExitCodeOutputChannel):
        return str(test.value)


def _add_channel(bundle: Bundle, output: OutputChannel, channel: Channel,
                 updates: List[Update]):
    """Add a channel to the output if it should be shown."""
    if should_show(output, channel):
        updates.append(StartTest(
            expected=guess_expected_value(bundle, output),
            channel=channel
        ))
        updates.append(CloseTest(
            generated="",
            status=StatusMessage(
                enum=Status.WRONG,
                human="Test niet uitgevoerd."
            ),
            accepted=False
        ))


def prepare_evaluation(bundle: Bundle, collector: OutputManager):
    """
    Generate output depicting the expected testplan. This output will be shown if
    the normal execution terminates early for some reason. This function assumes
    the output is OK, but does not accept anything.

    :param bundle: The configuration bundle.
    :param collector: The output collector.
    """
    collector.prepare_judgment(StartJudgment())
    for i, tab in enumerate(bundle.plan.tabs):
        collector.prepare_tab(StartTab(title=tab.name), i)
        for j, context in enumerate(tab.contexts):
            collector.prepare_context(
                StartContext(description=context.description), i, j
            )

            # Start with the context testcase.
            readable_input = get_readable_input(bundle, context.context_testcase)
            updates = [StartTestcase(description=readable_input)]

            # Do the normal output channels.
            output = context.context_testcase.output
            _add_channel(bundle, output.stdout, Channel.STDOUT, updates)
            _add_channel(bundle, output.stderr, Channel.STDERR, updates)
            _add_channel(bundle, output.file, Channel.FILE, updates)
            _add_channel(bundle, output.exception, Channel.EXCEPTION, updates)

            # Get exit code stuff, but don't evaluate yet.
            exit_output = output.exit_code

            # If this is the last testcase, do the exit code.
            if not context.testcases:
                _add_channel(bundle, exit_output, Channel.EXIT, updates)

            updates.append(AppendMessage("Testgeval niet uitgevoerd."))
            updates.append(CloseTestcase(accepted=False))

            # Begin normal testcases.
            for t, testcase in enumerate(context.testcases, 1):
                testcase: Testcase
                readable_input = get_readable_input(bundle, testcase)
                updates.append(StartTestcase(description=readable_input))

                # Do the normal output channels.
                output = testcase.output
                _add_channel(bundle, output.stdout, Channel.STDOUT, updates)
                _add_channel(bundle, output.stderr, Channel.STDERR, updates)
                _add_channel(bundle, output.file, Channel.FILE, updates)
                _add_channel(bundle, output.exception, Channel.EXCEPTION, updates)
                _add_channel(bundle, output.result, Channel.RETURN, updates)

                # If last testcase, do exit code.
                if t == len(context.testcases):
                    _add_channel(bundle, exit_output, Channel.EXIT, updates)

                updates.append(AppendMessage("Testgeval niet uitgevoerd."))
                updates.append(CloseTestcase(accepted=False))

            collector.prepare_context(updates, i, j)
            collector.prepare_context(CloseContext(accepted=False), i, j)
        collector.prepare_tab(CloseTab(), i)
    collector.prepare_judgment(CloseJudgment(accepted=False))
