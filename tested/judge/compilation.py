"""
Functions responsible for the compilation step.
"""
import logging
from pathlib import Path
from typing import List, Optional, Tuple, Union

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Message, Status
from tested.internationalization import get_i18n_string
from tested.judge.utils import BaseExecutionResult, run_command
from tested.languages.config import FileFilter, Language

_logger = logging.getLogger(__name__)


def run_compilation(
    bundle: Bundle, directory: Path, dependencies: List[Path], remaining: float
) -> Tuple[Optional[BaseExecutionResult], Union[List[str], FileFilter]]:
    """
    The compilation step in the pipeline. This callback is used in both the
    precompilation and individual mode. The implementation may only depend on
    the arguments.

    In individual compilation mode, this function may be called in a multi-
    threaded environment. Since the implementation is obvious to which mode
    it is operating in, it must be thread-safe.

    In individual mode, this function is responsible for compiling the code,
    such that a single context can be executed for evaluation. The compilation
    happens for each context, just before execution.

    In precompilation mode, the function is responsible for compiling all code
    at once. In some configs, this means the compilation will fail if one
    context is not correct. For those configs, the judge will fallback to
    individual compilation. This fallback does come with a heavy execution speed
    penalty, so disabling the fallback if not needed is recommended.

    :param bundle: The configuration bundle.
    :param directory: The directory in which the dependencies are
                              available and in which the compilation results
                              should be stored.
    :param dependencies: A list of files available for compilation. Some
                         configs might need a context_testcase file. By convention,
                         the last file is the context_testcase file.
    :param remaining: The max amount of time.

    :return: A tuple containing an optional compilation result, and a list of
             files, intended for further processing in the pipeline. For
             configs without compilation, the dependencies can be returned
             verbatim and without compilation results. Note that the judge might
             decide to fallback to individual mode if the compilation result is
             not positive.
    """
    command, files = bundle.lang_config.compilation([str(x) for x in dependencies])
    _logger.debug(
        "Generating files with command %s in directory %s", command, directory
    )
    result = run_command(directory, remaining, command)
    _logger.debug(f"Compilation dependencies are: {files}")
    return result, files


def process_compile_results(
    namespace: str, language_config: Language, results: Optional[BaseExecutionResult]
) -> Tuple[List[Message], Status, List[AnnotateCode]]:
    """
    Process the output of a compilation step. It will convert the result of the
    command into a list of messages and a status. If the status is not correct,
    the messages and status may be passed to Dodona unchanged. Alternatively, they
    can be kept to show them with the first context.
    """
    messages = []

    # There was no compilation
    if results is None:
        return messages, Status.CORRECT, []

    show_stdout = False
    _logger.debug("Received stderr from compiler: " + results.stderr)
    compiler_messages, annotations, stdout, stderr = language_config.compiler_output(
        namespace, results.stdout, results.stderr
    )
    messages.extend(compiler_messages)
    shown_messages = annotations or compiler_messages

    # Report stderr.
    if stderr:
        # Append compiler messages to the output.
        messages.append(get_i18n_string("judge.compilation.received.stderr"))
        messages.append(language_config.clean_stacktrace_to_message(stderr))
        _logger.debug("Received stderr from compiler: " + stderr)
        show_stdout = True
        shown_messages = True

    # Report stdout.
    if stdout and (show_stdout or results.exit != 0):
        # Append compiler messages to the output.
        messages.append(get_i18n_string("judge.compilation.received.stdout"))
        messages.append(language_config.clean_stacktrace_to_message(stdout))
        _logger.debug("Received stdout from compiler: " + stdout)
        shown_messages = True

    # Report errors if needed.
    if results.timeout:
        return messages, Status.TIME_LIMIT_EXCEEDED, annotations
    if results.memory:
        return messages, Status.MEMORY_LIMIT_EXCEEDED, annotations
    if results.exit != 0:
        if not shown_messages:
            messages.append(
                get_i18n_string("judge.compilation.exitcode", exitcode=results.exit)
            )
        return messages, Status.COMPILATION_ERROR, annotations
    else:
        return messages, Status.CORRECT, annotations
