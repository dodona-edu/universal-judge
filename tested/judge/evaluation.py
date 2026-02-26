import html
import logging
import urllib.parse
from collections.abc import Collection
from enum import StrEnum, unique
from pathlib import Path
from typing import Literal

from tested.configs import Bundle
from tested.dodona import (
    AppendMessage,
    CloseContext,
    CloseJudgement,
    CloseTab,
    CloseTest,
    CloseTestcase,
    EscalateStatus,
    ExtendedMessage,
    StartContext,
    StartTab,
    StartTest,
    StartTestcase,
    Status,
    StatusMessage,
    Update,
)
from tested.internationalization import get_i18n_string
from tested.judge.collector import OutputManager, TestcaseCollector
from tested.judge.execution import ContextResult
from tested.judge.planning import CompilationResult
from tested.languages.generation import (
    attempt_readable_input,
    generate_statement,
    get_readable_input,
)
from tested.oracles import get_oracle
from tested.oracles.common import OracleResult
from tested.testsuite import (
    ContentPath,
    Context,
    EmptyChannel,
    ExceptionOutput,
    ExceptionOutputChannel,
    ExitCodeOutputChannel,
    FileOutput,
    FileOutputChannel,
    IgnoredChannel,
    OutputChannel,
    SpecialOutputChannel,
    Testcase,
    TextData,
    TextOutput,
    TextOutputChannel,
    ValueOutput,
    ValueOutputChannel,
)
from tested.utils import safe_del, safe_get

_logger = logging.getLogger(__name__)


@unique
class Channel(StrEnum):
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
    actual: str | None,
    testcase: Testcase | None = None,
    unexpected_status: Status = Status.WRONG,
    timeout: bool = False,
    memory: bool = False,
) -> bool:
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

    :return: True indicates missing values.
    """
    evaluator = get_oracle(
        bundle, context_directory, output, testcase, unexpected_status=unexpected_status
    )
    # Run the oracle.
    # The file oracle produces multiple results if comparing multiple files.
    evaluation_results = evaluator(output, actual if actual else "")
    evaluation_results = (
        [evaluation_results]
        if not isinstance(evaluation_results, list)
        else evaluation_results
    )

    for evaluation_result in evaluation_results:
        status = evaluation_result.result

        # Decide if we should show this channel or not.
        is_correct = status.enum == Status.CORRECT
        should_report_case = should_show(output, channel, evaluation_result)

        if not should_report_case and is_correct:
            # We do report that a test is correct, to set the status.
            return False

        expected = evaluation_result.readable_expected
        if evaluation_result.channel_override is not None:
            display_channel = evaluation_result.channel_override
        else:
            display_channel = channel

        out.add(StartTest(expected=expected, channel=display_channel))

        # Report any messages we received.
        for message in evaluation_result.messages:
            out.add(AppendMessage(message=message))

        missing = False
        if actual is None:
            out.add(AppendMessage(message=get_i18n_string("judge.evaluation.missing")))
            missing = True
        elif should_report_case and timeout and not is_correct:
            status.human = get_i18n_string("judge.evaluation.time-limit")
            status.enum = Status.TIME_LIMIT_EXCEEDED
            out.add(AppendMessage(message=status.human))
        elif should_report_case and memory and not is_correct:
            status.human = get_i18n_string("judge.evaluation.memory-limit")
            status.enum = Status.TIME_LIMIT_EXCEEDED
            out.add(AppendMessage(message=status.human))

        # Close the test.
        out.add(CloseTest(generated=evaluation_result.readable_actual, status=status))

        if missing:
            return True

    return False


def evaluate_context_results(
    bundle: Bundle,
    context: Context,
    exec_results: ContextResult | None,
    compilation_results: CompilationResult,
    context_dir: Path,
    collector: OutputManager,
) -> Status | None:
    """
    Evaluate the results for a single context.

    This function is called for each context of the test suite, even if there
    are no results for the context in question.

    :param bundle: The configuration bundle.
    :param context: The context to evaluate.
    :param exec_results: The results of evaluating the context.
    :param compilation_results: The compiler results.
    :param context_dir: The directory where the execution happened.
    :param collector: Where to put the output
    :return: A status if of interest to the caller.
    """

    # If the compiler results are not successful, there is no point in doing more,
    # so stop early.
    if compilation_results.status != Status.CORRECT:
        readable_input = attempt_readable_input(bundle, context)
        collector.add(StartTestcase(description=readable_input))
        # Report all compiler messages.
        if not compilation_results.reported:
            collector.add_messages(compilation_results.messages)
            collector.add_all(compilation_results.annotations)
            collector.add(
                EscalateStatus(status=StatusMessage(enum=compilation_results.status))
            )

        # Finish the evaluation, since there is nothing we can do.
        collector.add(CloseTestcase(accepted=False), 0)
        return compilation_results.status

    # There must be execution if compilation succeeded.
    assert exec_results is not None

    # Split the basic output channels.
    stdout_ = exec_results.stdout.split(exec_results.separator)
    stderr_ = exec_results.stderr.split(exec_results.separator)
    exceptions = exec_results.exceptions.split(exec_results.separator)
    values = exec_results.results.split(exec_results.separator)

    # The first item should always be empty, since the separator must be printed
    # before the test suite runs. We remove the first item; but only
    # if it is indeed empty. This is to keep error messages present for
    # debugging.

    deletions = (
        safe_del(stdout_, 0, lambda e: e == ""),
        safe_del(stderr_, 0, lambda e: e == ""),
        safe_del(exceptions, 0, lambda e: e == ""),
        safe_del(values, 0, lambda e: e == ""),
    )

    could_delete = all(deletions)

    # Add a message indicating there were missing values.
    missing_values = None
    if not could_delete:
        _logger.warning("Missing output in context testcase.")
        missing_values = []
        missing_values.append(
            EscalateStatus(
                status=StatusMessage(
                    enum=Status.WRONG,
                    human=get_i18n_string("judge.evaluation.missing"),
                )
            )
        )
        # Recover stdout and stderr if present.
        if recovered := "\n".join(stdout_):
            missing_values.append(
                AppendMessage(
                    message=ExtendedMessage(
                        description="Standaarduitvoer was:\n" + recovered, format="code"
                    )
                )
            )
        if recovered := "\n".join(stderr_):
            missing_values.append(
                AppendMessage(
                    message=ExtendedMessage(
                        description="Standaardfout was:\n" + recovered, format="code"
                    )
                )
            )

    # All files that will be used in this context.
    all_files = context.get_files()

    # Begin processing the normal testcases.
    for i, testcase in enumerate(context.testcases):
        _logger.debug(f"Evaluating testcase {i}")

        readable_input, seen = get_readable_input(bundle, testcase)
        all_files = all_files - seen
        t_col = TestcaseCollector(StartTestcase(description=readable_input))

        # Get the functions
        output = testcase.output

        # Get the values produced by the execution. If there are no values,
        # we use an empty string at this time. We handle missing output later.
        actual_stderr = safe_get(stderr_, i)
        actual_exception = safe_get(exceptions, i)
        actual_stdout = safe_get(stdout_, i)
        actual_value = safe_get(values, i)

        missing_file = _evaluate_channel(
            bundle,
            context_dir,
            t_col,
            Channel.FILE,
            output.file,
            "",
            timeout=exec_results.timeout,
            memory=exec_results.memory,
        )
        missing_stderr = _evaluate_channel(
            bundle,
            context_dir,
            t_col,
            Channel.STDERR,
            output.stderr,
            actual_stderr,
            timeout=exec_results.timeout and len(stderr_) == i + 1,
            memory=exec_results.memory and len(stderr_) == i + 1,
        )
        missing_exception = _evaluate_channel(
            bundle,
            context_dir,
            t_col,
            Channel.EXCEPTION,
            output.exception,
            actual_exception,
            unexpected_status=Status.RUNTIME_ERROR,
            timeout=exec_results.timeout and len(exceptions) == i + 1,
            memory=exec_results.memory and len(exceptions) == i + 1,
        )
        missing_stdout = _evaluate_channel(
            bundle,
            context_dir,
            t_col,
            Channel.STDOUT,
            output.stdout,
            actual_stdout,
            timeout=exec_results.timeout and len(stdout_) == i + 1,
            memory=exec_results.memory and len(stdout_) == i + 1,
        )
        missing_return = _evaluate_channel(
            bundle,
            context_dir,
            t_col,
            Channel.RETURN,
            output.result,
            actual_value,
            testcase=testcase,
            timeout=exec_results.timeout and len(values) == i + 1,
            memory=exec_results.memory and len(values) == i + 1,
        )

        # If this is the last testcase, do the exit channel.
        if i == len(context.testcases) - 1:
            missing_exit = _evaluate_channel(
                bundle,
                context_dir,
                t_col,
                Channel.EXIT,
                testcase.output.exit_code,
                str(exec_results.exit),
                timeout=exec_results.timeout,
                memory=exec_results.memory,
            )
        else:
            missing_exit = False
            assert (
                testcase.output.exit_code == IgnoredChannel.IGNORED
            ), "Only the last testcase may check the exit code."

        # Add messages if there was no output.
        if missing_values is not None:
            if not (
                missing_file
                or missing_stderr
                or missing_exception
                or missing_stdout
                or missing_return
                or missing_exit
            ):
                t_col.add(
                    AppendMessage(message=get_i18n_string("judge.evaluation.missing"))
                )
            for u in missing_values:
                t_col.add(u)

        t_col.to_manager(collector, CloseTestcase(), i)

    # Add file links
    if all_files:
        collector.add(link_files_message(all_files))

    if exec_results.timeout:
        return Status.TIME_LIMIT_EXCEEDED
    if exec_results.memory:
        return Status.MEMORY_LIMIT_EXCEEDED
    return None


def link_files_message(
    link_files: Collection[TextData],
) -> AppendMessage:
    link_list = []
    for link_file in link_files:
        # TODO: handle inline files somehow.
        if link_file.path is not None and isinstance(link_file.content, ContentPath):
            the_url = urllib.parse.quote(link_file.content.path)
            link_list.append(
                f'<a href="{the_url}" class="file-link" target="_blank">'
                f'<span class="code">{html.escape(link_file.path)}</span></a>'
            )

    file_list = ", ".join(link_list)
    file_list_str = get_i18n_string(
        "judge.evaluation.files", count=len(link_list), files=file_list
    )
    description = f"<div class='contains-file'><p>{file_list_str}</p></div>"
    message = ExtendedMessage(description=description, format="html")
    return AppendMessage(message=message)


def should_show(
    test: OutputChannel, channel: Channel, result: OracleResult | None = None
) -> bool:
    """
    Determine if the channel should be shown, without accounting for the actual
    value. This function answers the question: "Assuming the actual value is
    correct, should we show this output channel?".

    :param test: The output for the channel from the test suite.
    :param channel: The channel.
    :param result: The result of the evaluation.

    :return: True if the channel should be shown, false otherwise.
    """
    if channel == Channel.EXIT:
        if test == IgnoredChannel.IGNORED:
            return False
        assert isinstance(test, ExitCodeOutputChannel)
        return test.value != 0
    elif channel in (Channel.STDOUT, Channel.STDERR):
        assert isinstance(test, TextOutput)
        # We don't show the channel if the output is nothing or ignored.
        return not isinstance(test, SpecialOutputChannel)
    elif channel == Channel.FILE:
        assert isinstance(test, FileOutput)
        # We don't show the channel if we ignore the channel.
        return not isinstance(test, IgnoredChannel)
    elif channel == Channel.RETURN:
        assert isinstance(test, ValueOutput)
        # We don't show the channel if we ignore it.
        if isinstance(test, IgnoredChannel):
            return False
        if (
            isinstance(test, EmptyChannel)
            and result
            and result.result.enum == Status.CORRECT
        ):
            return False
        return True
    elif channel == Channel.EXCEPTION:
        assert isinstance(test, ExceptionOutput)
        return not isinstance(test, SpecialOutputChannel)
    else:
        raise AssertionError(f"Unknown channel {channel}")


def guess_expected_value(
    bundle: Bundle, test: OutputChannel, file_index: int | None = None
) -> str:
    """
    Try and get the expected value for an output channel. In some cases, such as
    a programmed or language-specific oracle, there will be no expected value
    available in the test suite. In that case, we use an empty string.

    :param bundle: Configuration bundle.
    :param test: The output channel.
    :param file_index: Index of the file in the output channel, if applicable.

    :return: A best effort attempt of the expected value.
    """
    if isinstance(test, SpecialOutputChannel):
        return ""
    elif isinstance(test, TextOutputChannel):
        return test.get_data_as_string(bundle.config.resources)
    elif isinstance(test, FileOutputChannel):
        # We know file index will be set when we have a FileOutputChannel.
        return test.get_data_as_string(bundle.config.resources, file_index or 0)
    elif isinstance(test, ExceptionOutputChannel):
        return (
            test.exception.readable(bundle.config.programming_language)
            if test.exception
            else get_i18n_string("judge.evaluation.dynamic")
        )
    elif isinstance(test, ValueOutputChannel):
        return (
            generate_statement(bundle, test.value)
            if test.value
            else get_i18n_string("judge.evaluation.dynamic")
        )
    elif isinstance(test, ExitCodeOutputChannel):
        return str(test.value)
    _logger.warning(f"Unknown output type {test}")
    return ""


def _add_channel(
    bundle: Bundle, output: OutputChannel, channel: Channel, updates: list[Update]
):
    """Add a channel to the output if it should be shown."""
    if should_show(output, channel):
        if isinstance(output, FileOutputChannel):
            for i, file in enumerate(output.files):
                if file.path is None:
                    continue

                updates.append(
                    StartTest(
                        expected=guess_expected_value(bundle, output), channel=file.path
                    )
                )
                updates.append(
                    CloseTest(
                        generated="",
                        status=StatusMessage(enum=Status.NOT_EXECUTED),
                        accepted=False,
                    )
                )
        else:
            updates.append(
                StartTest(
                    expected=guess_expected_value(bundle, output), channel=channel
                )
            )
            updates.append(
                CloseTest(
                    generated="",
                    status=StatusMessage(enum=Status.NOT_EXECUTED),
                    accepted=False,
                )
            )


def complete_evaluation(bundle: Bundle, collector: OutputManager):
    # We assume we have at least an open judgement.
    if collector.finalized:
        return

    assert (
        "judgement" in collector.open_stack
    ), "A non-finalized output manager without open judgement is not possible."

    tab_start, context_start, testcase_start = collector.currently_open

    for tab in bundle.suite.tabs[tab_start:]:
        if context_start == 0 and testcase_start == 0:
            collector.add(StartTab(title=tab.name, hidden=tab.hidden))
        assert tab.contexts
        for context in tab.contexts[context_start:]:
            updates: list[Update] = [
                AppendMessage(message=get_i18n_string("judge.evaluation.not-executed"))
            ]
            if testcase_start == 0:
                collector.add(StartContext(description=context.description))
            # All files that will be used in this context.
            all_files = context.get_files()

            # Begin normal testcases.
            for j, testcase in enumerate(
                context.testcases[testcase_start:], start=testcase_start
            ):
                readable_input, seen = get_readable_input(bundle, testcase)
                all_files = all_files - seen
                updates.append(StartTestcase(description=readable_input))

                # Do the normal output channels.
                output = testcase.output
                _add_channel(bundle, output.stdout, Channel.STDOUT, updates)
                _add_channel(bundle, output.stderr, Channel.STDERR, updates)
                _add_channel(bundle, output.file, Channel.FILE, updates)
                _add_channel(bundle, output.exception, Channel.EXCEPTION, updates)
                _add_channel(bundle, output.result, Channel.RETURN, updates)

                # If last testcase, do exit code.
                if j == len(tab.contexts) - 1:
                    _add_channel(bundle, output.exit_code, Channel.EXIT, updates)

                updates.append(CloseTestcase(accepted=False))
            testcase_start = 0  # For the next context, start at the beginning

            # Add links to files we haven't seen yet.
            if all_files:
                updates.insert(0, link_files_message(all_files))

            collector.add_all(updates)
            collector.add(CloseContext(accepted=False))
        collector.add(CloseTab())
        context_start = 0  # For the next tab, start from the beginning.
    collector.add(CloseJudgement())


def terminate(
    bundle: Bundle,
    collector: OutputManager,
    status_if_unclosed: Status | StatusMessage,
):
    # Determine the level we need to close.
    tab, context, testcase = collector.currently_open
    max_tab = len(bundle.suite.tabs)

    until: Literal["testcase", "context", "tab", "judgement"]
    if tab == max_tab:
        # We are basically done.
        until = "judgement"
    elif context == len(bundle.suite.tabs[tab].contexts):
        # We have done all contexts here, so close the current tab.
        until = "tab"
    elif testcase == len(bundle.suite.tabs[tab].contexts[context].testcases):
        # We have done all testcases, so close the current context.
        until = "context"
    else:
        until = "testcase"

    collector.terminate(status_if_unclosed=status_if_unclosed, until=until)
    complete_evaluation(bundle, collector)
