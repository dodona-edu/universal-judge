import logging
import shutil
from pathlib import Path
from typing import Tuple, List

from .compilation import run_compilation, process_compile_results
from .execution import ContextExecution, ExecutionResult, execute_file
from .utils import find_main_file
from ..configs import Bundle
from ..dodona import *
from ..evaluators import Evaluator, get_evaluator
from ..languages.generator import get_readable_input
from ..languages.paths import value_file, exception_file
from ..testplan import Context, ExecutionMode, OutputChannel, EmptyChannel, \
    IgnoredChannel, ExitCodeOutputChannel, Testcase, ContextTestcase

_logger = logging.getLogger(__name__)


def _evaluate_channel(
        out: Union[UpdateCollector, IO],
        channel_name: str,
        expected_output: OutputChannel,
        actual_result: str,
        evaluator: Evaluator) -> bool:
    """
    Evaluate the output on a given channel. This function will output the
    appropriate messages
    to start and end a new test in Dodona.

    If errors is given, the test will end with a runtime error. Note that if the
    channel output
    is None, the test will not be written to Dodona if everything is correct.

    :param out: The output file for the judge.
    :param channel_name: The name of the channel being evaluated. Will be
                         displayed in Dodona.
    :param expected_output: The output channel from the test case.
    :param actual_result: The actual output.
    :param evaluator: The evaluator to use.
    :return: True if successful, otherwise False.
    """
    evaluation_result = evaluator(expected_output, actual_result)
    status = evaluation_result.result

    test_collector = UpdateCollector(StartTest(
        expected=evaluation_result.readable_expected,
        channel=channel_name
    ))

    # If the actual value is empty and the channel output is None or ignored,
    # don't report it.
    is_correct = status.enum == Status.CORRECT
    has_no_result = actual_result is None or actual_result == ""
    has_no_expected = (expected_output == EmptyChannel.NONE
                       or expected_output == IgnoredChannel.IGNORED)
    is_exit_code = isinstance(expected_output, ExitCodeOutputChannel)
    if is_correct and ((has_no_result and has_no_expected) or is_exit_code):
        return True

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

    # Actual do the evaluation.
    results = [
        _evaluate_channel(
            context_collector, "file", testcase.output.file, "", file_evaluator
        ),
        _evaluate_channel(
            context_collector, "stderr", testcase.output.stderr, actual_stderr,
            stderr_evaluator
        ),
        _evaluate_channel(
            context_collector, "exception", testcase.output.exception,
            actual_exception, exception_evaluator
        ),
        _evaluate_channel(
            context_collector, "stdout", testcase.output.stdout, actual_stdout,
            stdout_evaluator
        )
    ]

    # Check for missing values and stop if necessary.
    if not stdout_ or not stderr_ or not exceptions or not values:
        context_collector.collect(AppendMessage(
            "Ontbrekende uitvoerresultaten in Dodona. Er ging iets verkeerd!"
        ))
        results.append(False)  # Ensure we stop.

    must_stop = False
    if not all(results):
        # As last item, we evaluate the exit code of the context.
        _evaluate_channel(context_collector, "exitcode", exit_output,
                          str(exec_results.exit), exit_evaluator)
        must_stop = True

    # Done with the context testcase.
    context_collector.end(bundle.out, CloseTestcase())

    # Decide if we want to proceed.
    if must_stop:
        return  # Stop now.

    # Begin processing the normal testcases.
    for i, testcase in enumerate(context.testcases):
        # Type hint for PyCharm.
        testcase: Testcase

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
            _evaluate_channel(
                bundle.out, "file", testcase.output.file, "", file_evaluator
            ),
            _evaluate_channel(
                bundle.out, "stderr", testcase.output.stderr, actual_stderr,
                stderr_evaluator
            ),
            _evaluate_channel(
                bundle.out, "exception", testcase.output.exception,
                actual_exception, exception_evaluator
            ),
            _evaluate_channel(
                bundle.out, "stdout", testcase.output.stdout, actual_stdout,
                stdout_evaluator
            ),
            _evaluate_channel(
                bundle.out, "return", testcase.output.result, actual_value,
                value_evaluator
            )
        ]

        # Check for missing values and stop if necessary.
        if (i >= len(stdout_)
                or i >= len(stderr_)
                or i >= len(values)
                or i >= len(exceptions)):
            report_update(bundle.out, AppendMessage(
                "Ontbrekende uitvoerresultaten in Dodona. Er ging iets verkeerd!"
            ))
            super_stop = True
        else:
            super_stop = False

        # Decide if we want to proceed.
        if (testcase.essential and not all(results)) or super_stop:
            # As last item, we evaluate the exit code of the context.
            _evaluate_channel(bundle.out, "exitcode", exit_output,
                              str(exec_results.exit), exit_evaluator)
            must_stop = True

        report_update(bundle.out, CloseTestcase())

        if must_stop:
            return  # Stop evaluation now.


def execute_context(bundle: Bundle, args: ContextExecution) \
        -> Tuple[Optional[ExecutionResult], List[Message], Status, Path]:
    """
    Execute a context.
    """
    lang_config = bundle.language_config

    # Create a working directory for the context.
    context_dir = Path(
        bundle.config.workdir,
        args.context_name
    )
    context_dir.mkdir()

    _logger.info("Executing context %s in path %s",
                 args.context_name, context_dir)

    dependencies = lang_config.context_dependencies_callback(
        args.context_name,
        args.files
    )

    # Copy files from the common directory to the context directory.
    for file in dependencies:
        origin = args.common_directory / file
        _logger.debug("Copying %s to %s", origin, context_dir)
        # noinspection PyTypeChecker
        shutil.copy2(origin, context_dir)

    # If needed, do a compilation.
    if args.mode == ExecutionMode.INDIVIDUAL:
        _logger.info("Compiling context %s in INDIVIDUAL mode...",
                     args.context_name)
        result, files = run_compilation(bundle, context_dir, dependencies)

        # Process compilation results.
        messages, status, annotations = process_compile_results(
            lang_config,
            result
        )

        for annotation in annotations:
            report_update(bundle.out, annotation)

        if status != Status.CORRECT:
            _logger.debug("Compilation of individual context failed.")
            _logger.debug("Aborting executing of this context.")
            return None, messages, status, context_dir

        _logger.debug("Executing context %s in INDIVIDUAL mode...",
                      args.context_name)
        executable = find_main_file(files, args.context_name)
        files.remove(executable)
        stdin = args.context.get_stdin(bundle.config.resources)

        base_result = execute_file(
            bundle,
            executable_name=executable,
            working_directory=context_dir,
            dependencies=files,
            stdin=stdin
        )
    else:
        result, files = None, dependencies
        if args.precompilation_result:
            _logger.debug("Substituting precompilation results.")
            messages, status = args.precompilation_result
        else:
            _logger.debug("No precompilation results found, using default.")
            messages, status = [], Status.CORRECT

        _logger.info("Executing context %s in PRECOMPILATION mode...",
                     args.context_name)

        if lang_config.needs_selector():
            _logger.debug("Selector is needed, using it.")

            selector_name = lang_config.selector_name()
            executable = find_main_file(files, selector_name)
            files.remove(executable)
            stdin = args.context.get_stdin(bundle.config.resources)

            base_result = execute_file(
                bundle,
                executable_name=executable,
                working_directory=context_dir,
                dependencies=files,
                stdin=stdin,
                argument=args.context_name
            )
        else:
            _logger.debug("Selector is not needed, using individual execution.")
            executable = find_main_file(files, args.context_name)
            files.remove(executable)
            stdin = args.context.get_stdin(bundle.config.resources)

            base_result = execute_file(
                bundle,
                executable_name=executable,
                working_directory=context_dir,
                dependencies=files,
                stdin=stdin
            )

    identifier = f"--{bundle.secret}-- SEP"

    value_file_path = value_file(bundle, context_dir)
    try:
        with open(value_file_path, "r") as f:
            values = f.read()
    except FileNotFoundError:
        _logger.warning("Value file not found, looked in %s", value_file_path)
        values = ""

    exception_file_path = exception_file(bundle, context_dir)
    try:
        # noinspection PyTypeChecker
        with open(exception_file_path, "r") as f:
            exceptions = f.read()
    except FileNotFoundError:
        _logger.warning("Exception file not found, looked in %s",
                        exception_file_path)
        exceptions = ""

    result = ExecutionResult(
        stdout=base_result.stdout,
        stderr=base_result.stderr,
        exit=base_result.exit,
        separator=identifier,
        results=values,
        exceptions=exceptions
    )

    return result, messages, status, context_dir
