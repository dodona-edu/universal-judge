"""
Common utilities for the judge.
"""
import subprocess

from pydantic.dataclasses import dataclass
from typing import List, Optional
from pathlib import Path

from judge import _logger


@dataclass
class BaseExecutionResult:
    """
    Base result of executing a command.
    """
    stdout: str
    stderr: str
    exit: int


def run_command(directory: Path,
                command: Optional[List[str]] = None,
                stdin: Optional[str] = None) -> Optional[BaseExecutionResult]:
    """
    Run a command and get the result of said command.

    :param directory: The directory to execute in.
    :param command: Optional, the command to execute.
    :param stdin: Optional stdin for the process.

    :return: The result of the execution if the command was not None.
    """
    if not command:
        return None

    process = subprocess.run(command, cwd=directory, text=True, capture_output=True,
                             input=stdin)
    return BaseExecutionResult(
        stdout=process.stdout,
        stderr=process.stderr,
        exit=process.returncode
    )


def find_main_file(files: List[str], name: str) -> str:
    _logger.debug("Finding %s in %s", name, files)
    return [x for x in files if x.startswith(name)][0]
