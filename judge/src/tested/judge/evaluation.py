import shutil
from pathlib import Path
from typing import IO, Union, Tuple, List, Optional

from tested.dodona import Status, report_update, StartTest, AppendMessage, CloseTest, \
    Message, StartTestcase, EscalateStatus, StatusMessage, CloseTestcase, \
    ExtendedMessage, Permission
from ..evaluators import Evaluator, get_evaluator
from . import ExecutionResult, ContextExecution, _logger, run_compilation, \
    _process_compile_results, find_main_file, execute_file
from ..languages.generator import get_readable_input
from ..languages.paths import value_file, exception_file
from ..configs import Bundle
from ..testplan import Context, ExecutionMode
from ..testplan.channels import OutputChannel, EmptyChannel, IgnoredChannel, \
    ExitCodeOutputChannel
from ..testplan.testcase import Testcase
from ..testplan.utils import TestPlanError


def _evaluate_channel(
        out: IO,
        channel_name: str,
        expected_output: OutputChannel,
        actual_result: Union[str, int, None],
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
    :param actual_result: The actual output. Can be None, depending on the
    evaluator.
    :param evaluator: The evaluator to use.
    :return: True if successful, otherwise False.
    """
    evaluation_result = evaluator(expected_output, actual_result)
    status = evaluation_result.result

    # If the actual value is empty and the channel output is None or ignored,
    # don't report it.
    is_correct = status.enum == Status.CORRECT
    has_no_result = actual_result is None or actual_result == ""
    has_no_expected = (expected_output == EmptyChannel.NONE
                       or expected_output == IgnoredChannel.IGNORED)
    is_exit_code = isinstance(expected_output, ExitCodeOutputChannel)
    if is_correct and ((has_no_result and has_no_expected) or is_exit_code):
        return True

    report_update(out, StartTest(
        expected=evaluation_result.readable_expected,
        channel=channel_name
    ))

    # Report any messages we received.
    for message in evaluation_result.messages:
        report_update(out, AppendMessage(message=message))

    # Close the test.
    report_update(out, CloseTest(
        generated=evaluation_result.readable_actual,
        status=status
    ))

    return is_correct


def evaluate_results(bundle: Bundle,
                     context: Context,
                     exec_results: ExecutionResult,
                     compiler_results: Tuple[List[Message], Status],
                     context_dir: Path):
    # Process output
    stdout_ = exec_results.stdout.split(exec_results.separator) \
        if exec_results is not None else []
    stderr_ = exec_results.stderr.split(exec_results.separator) \
        if exec_results is not None else []
    values = exec_results.results.split(exec_results.separator) \
        if exec_results is not None else []
    exceptions = exec_results.exceptions.split(exec_results.separator) \
        if exec_results is not None else []

    # There might be less output than testcase, which is an error. However,
    # we process the
    # output we have, to ensure we send as much feedback as possible to the
    # user.
    testcase: Testcase  # Type hint for pycharm.
    # TODO: handle context testcase
    for i, testcase in enumerate(context.testcases):

        name = bundle.language_config.submission_name(bundle.plan)
        readable_input = get_readable_input(bundle, testcase)
        report_update(bundle.out, StartTestcase(description=readable_input))

        # Handle compiler results
        if compiler_results[1] != Status.CORRECT:
            for message in compiler_results[0]:
                report_update(bundle.out, AppendMessage(message=message))
            report_update(bundle.out, EscalateStatus(
                status=StatusMessage(enum=compiler_results[1])))
            report_update(bundle.out, CloseTestcase(accepted=False))
            break
        else:
            assert exec_results is not None

        results = []
        # We don't stop if the exit code, to show the other issues.

        # Get the evaluators
        try:
            stdout_evaluator = get_evaluator(bundle, context_dir, testcase.output.stdout)
            stderr_evaluator = get_evaluator(bundle, context_dir, testcase.output.stderr)
            file_evaluator = get_evaluator(bundle, context_dir, testcase.output.file)
            value_evaluator = get_evaluator(bundle, context_dir, testcase.output.result)
            exception_evaluator = get_evaluator(bundle, context_dir, testcase.output.exception)
        except TestPlanError as e:
            report_update(bundle.out, AppendMessage(message=ExtendedMessage(
                description=str(e),
                format='text',
                permission=Permission.STAFF
            )))
            break

        # Evaluate the file channel.
        results.append(_evaluate_channel(
            bundle.out, "file", testcase.output.file, None, file_evaluator
        ))

        # Evaluate the stderr channel
        actual_stderr = stderr_[i] if i < len(stderr_) else None
        results.append(_evaluate_channel(
            bundle.out, "stderr", testcase.output.stderr, actual_stderr,
            stderr_evaluator
        ))

        actual_exception = exceptions[i] if i < len(exceptions) else None
        results.append(_evaluate_channel(
            bundle.out, "exception", testcase.output.exception, actual_exception,
            exception_evaluator
        ))

        actual_stdout = stdout_[i] if i < len(stdout_) else None
        results.append(_evaluate_channel(
            bundle.out, "stdout", testcase.output.stdout, actual_stdout,
            stdout_evaluator
        ))

        actual_value = values[i] if i < len(values) else None
        results.append(
            _evaluate_channel(bundle.out, "return", testcase.output.result,
                              actual_value, value_evaluator)
        )

        # Check if there is early termination.
        if i >= len(stdout_) \
                or i >= len(stderr_) \
                or i >= len(values) \
                or i >= len(exceptions):
            report_update(bundle.out, AppendMessage(message=ExtendedMessage(
                description="Tests were terminated early.", format='text'
            )))

        report_update(bundle.out, CloseTestcase())

        # If this was an essential testcase with an error, stop testing now.
        if testcase.essential and not all(results):
            break


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
        messages, status, annotations = _process_compile_results(
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
