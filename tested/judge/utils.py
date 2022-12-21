"""
Common utilities for the judge.
"""
import logging
import shutil
import subprocess
from pathlib import Path
from typing import List, Optional

from pydantic.dataclasses import dataclass

_logger = logging.getLogger(__name__)


@dataclass
class BaseExecutionResult:
    """
    Base result of executing a command.
    """

    stdout: str
    stderr: str
    exit: int
    timeout: bool
    memory: bool


def run_command(
    directory: Path,
    timeout: Optional[float],
    command: Optional[List[str]] = None,
    stdin: Optional[str] = None,
) -> Optional[BaseExecutionResult]:
    """
    Run a command and get the result of said command.

    :param directory: The directory to execute in.
    :param command: Optional, the command to execute.
    :param stdin: Optional stdin for the process.
    :param timeout: The max time for this command.

    :return: The result of the execution if the command was not None.
    """
    if not command:
        return None

    try:
        timeout = int(timeout) if timeout is not None else None
        # noinspection PyTypeChecker
        process = subprocess.run(
            command,
            cwd=directory,
            text=True,
            errors="backslashreplace",
            capture_output=True,
            input=stdin,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired as e:
        return BaseExecutionResult(
            stdout=e.stdout or "",
            stderr=e.stderr or "",
            exit=0,
            timeout=True,
            memory=False,
        )

    return BaseExecutionResult(
        stdout=process.stdout,
        stderr=process.stderr,
        exit=process.returncode,
        timeout=False,
        memory=True if process.returncode == -9 else False,
    )


def copy_from_paths_to_path(origins: List[Path], files: List[str], destination: Path):
    """
    Copy a list of files from a list of source folders to a destination folder. The
    source folders are searched in-order for the name of the file, which means that
    if multiple folders contain a file with the same name, only the first one will
    be used. Conversely, if no source folder contains a file with a requested name,
    an error will be thrown.
    :param origins: The source folders to copy from.
    :param files: The files to copy.
    :param destination: The destination folder to copy to.
    """
    # Copy files to the common directory.
    files_to_copy = []
    for file in files:
        for potential_path in origins:
            if (full_file := potential_path / file).exists():
                files_to_copy.append(full_file)
                break
        else:  # no break
            raise ValueError(
                f"Could not find dependency file {file}, " f"looked in {origins}"
            )
    for file in files_to_copy:
        # noinspection PyTypeChecker
        shutil.copy2(file, destination)
