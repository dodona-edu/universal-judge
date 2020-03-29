"""
Common utilities for the judge.
"""
import logging
import shutil
import subprocess

from pydantic.dataclasses import dataclass
from typing import List, Optional
from pathlib import Path

_logger = logging.getLogger(__name__)


@dataclass
class BaseExecutionResult:
    """
    Base result of executing a command.
    """
    stdout: str
    stderr: str
    exit: int
    was_timeout: bool


def run_command(directory: Path,
                command: Optional[List[str]] = None,
                stdin: Optional[str] = None,
                timout: Optional[int] = None) -> Optional[BaseExecutionResult]:
    """
    Run a command and get the result of said command.

    :param directory: The directory to execute in.
    :param command: Optional, the command to execute.
    :param stdin: Optional stdin for the process.
    :param timout:

    :return: The result of the execution if the command was not None.
    """
    if not command:
        return None

    try:
        # noinspection PyTypeChecker
        process = subprocess.run(command, cwd=directory, text=True,
                                 capture_output=True, input=stdin, timeout=timout)
    except subprocess.TimeoutExpired as e:
        return BaseExecutionResult(
            stdout=e.stdout,
            stderr=e.stderr,
            exit=0,  # Use 0 to prevent double reporting.
            was_timeout=True
        )
    return BaseExecutionResult(
        stdout=process.stdout,
        stderr=process.stderr,
        exit=process.returncode,
        was_timeout=False
    )


def find_main_file(files: List[str], name: str) -> str:
    _logger.debug("Finding %s in %s", name, files)
    return [x for x in files if x.startswith(name)][0]


def copy_from_paths_to_path(origins: List[Path],
                            files: List[str],
                            destination: Path):
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
            raise ValueError(f"Could not find dependency file {file}, "
                             f"looked in {origins}")
    for file in files_to_copy:
        # noinspection PyTypeChecker
        shutil.copy2(file, destination)
