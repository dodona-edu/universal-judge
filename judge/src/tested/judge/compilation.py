"""
Functions responsible for the compilation step.
"""
import logging
from pathlib import Path
from typing import List, Tuple, Optional

from .utils import run_command, BaseExecutionResult
from ..configs import Bundle
from ..dodona import Message, Status, AnnotateCode, ExtendedMessage
from ..languages.config import Language

_logger = logging.getLogger(__name__)


def run_compilation(bundle: Bundle,
                    directory: Path,
                    dependencies: List[str],
                    remaining: float
                    ) -> Tuple[Optional[BaseExecutionResult], List[str]]:
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
                         TODO: make this explicit?
    :param remaining: The max amount of time.

    :return: A tuple containing an optional compilation result, and a list of
             files, intended for further processing in the pipeline. For
             configs without compilation, the dependencies can be returned
             verbatim and without compilation results. Note that the judge might
             decide to fallback to individual mode if the compilation result is
             not positive.
    """
    command, files = bundle.language_config.c_compilation(dependencies)
    _logger.debug("Generating files with command %s in directory %s",
                  command, directory)
    result = run_command(directory, remaining, command)
    _logger.debug(f"Would take files: {files}")
    return result, files


def process_compile_results(
        language_config: Language,
        results: Optional[BaseExecutionResult]
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
    compiler_messages, annotations, stdout, stderr = \
        language_config.c_compiler_output(results.stdout, results.stderr)
    messages.extend(compiler_messages)
    shown_messages = annotations or compiler_messages

    # Report stderr.
    if stderr:
        # Append compiler messages to the output.
        messages.append("De compiler produceerde volgende uitvoer op stderr:")
        messages.append(ExtendedMessage(
            description=stderr,
            format='code'
        ))
        _logger.debug("Received stderr from compiler: " + stderr)
        show_stdout = True
        shown_messages = True

    # Report stdout.
    if stdout and (show_stdout or results.exit != 0):
        # Append compiler messages to the output.
        messages.append("De compiler produceerde volgende uitvoer op stdout:")
        messages.append(ExtendedMessage(
            description=stdout,
            format='code'
        ))
        _logger.debug("Received stdout from compiler: " + stderr)
        shown_messages = True

    # Report errors if needed.
    if results.exit != 0:
        if not shown_messages:
            # TODO: perhaps this should be the status message?
            messages.append(f"Exitcode {results.exit}.")
        return messages, Status.COMPILATION_ERROR, annotations
    else:
        return messages, Status.CORRECT, annotations
