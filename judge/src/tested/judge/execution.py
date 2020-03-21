import logging
from pathlib import Path
from typing import List, Optional, Tuple

from pydantic.dataclasses import dataclass

from tested.dodona import Message, Status
from .utils import BaseExecutionResult, run_command
from ..configs import Bundle
from ..testplan import Context, ExecutionMode

_logger = logging.getLogger(__name__)


@dataclass
class ExecutionResult(BaseExecutionResult):
    """
    The result of a context_testcase testcase execution.

    All output streams are divided per testcase, in the same order as the
    context that was used to execute_module the test. E.g. the string at position
    0 in
    stdout is the result of executing the testcase at position 0 in the context.
    """
    separator: str
    results: str
    exceptions: str


@dataclass
class ContextExecution:
    """
    Arguments used to execute_module a single context of the testplan.
    """
    context: Context
    context_name: str
    mode: ExecutionMode
    common_directory: Path
    files: List[str]
    precompilation_result: Optional[Tuple[List[Message], Status]]


def execute_file(
        bundle: Bundle,
        executable_name: str,
        working_directory: Path,
        dependencies: List[str],
        stdin: Optional[str] = None,
        argument: Optional[str] = None
) -> BaseExecutionResult:
    """
    Execute a file.

    Note that this method must be thread-safe.

    :param bundle: The configuration bundle.
    :param dependencies: A list of files that are available in the given working
                         directory.
    :param working_directory: The working directory, in which the execution must
                              take place.
    :param argument: Argument for the executable, optional.
    :param stdin: The stdin for the execution.
    :param executable_name: The executable that should be executed. This file
                            will not be present in the dependency list.

    :return: The result of the execution.
    """
    _logger.info("Starting execution on file %s", executable_name)

    command = bundle.language_config.execution_command(
        cwd=working_directory,
        file=executable_name,
        dependencies=dependencies,
        arguments=[argument] if argument else []
    )
    _logger.debug("Executing command %s in directory %s", command,
                  working_directory)

    result = run_command(working_directory, command, stdin)
    assert result is not None
    return result
