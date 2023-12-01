"""
Common utilities for the judge.
"""
import logging
import shutil
import subprocess
from pathlib import Path

from attrs import define

from tested.configs import Bundle
from tested.languages.config import FileFilter
from tested.languages.conventionalize import EXECUTION_PREFIX

_logger = logging.getLogger(__name__)


@define
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
    timeout: float | None,
    command: list[str] | None = None,
    stdin: str | None = None,
) -> BaseExecutionResult | None:
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
            stdout=e.stdout.decode("utf-8", "backslashreplace") if e.stdout else "",
            stderr=e.stderr.decode("utf-8", "backslashreplace") if e.stderr else "",
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


def copy_from_paths_to_path(origins: list[Path], files: list[str], destination: Path):
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
        shutil.copy2(file, destination)


def copy_workdir_files(bundle: Bundle, destination: Path, all_files: bool) -> list[str]:
    """
    Copy files from the workdir to a destination.

    :param bundle: Bundle information of the test suite
    :param destination: Where to copy to.
    :param all_files: If all files or only source files should be copied.
    """
    source_files = []

    def recursive_copy(src: Path, dst: Path):
        for origin in src.iterdir():
            file = origin.name.lower()
            if origin.is_file() and (
                all_files or bundle.language.is_source_file(origin)
            ):
                source_files.append(str(dst / origin.name))
                _logger.debug(f"Copying {origin} to {dst}")
                shutil.copy2(origin, dst)
            elif (
                origin.is_dir()
                and not file.startswith(EXECUTION_PREFIX)
                and file != "common"
            ):
                _logger.debug(f"Iterate subdir {dst / file}")
                shutil.copytree(origin, dst / file)

    recursive_copy(bundle.config.workdir, destination)

    return source_files


def filter_files(files: list[str] | FileFilter, directory: Path) -> list[Path]:
    if callable(files):
        return list(
            x.relative_to(directory) for x in filter(files, directory.rglob("*"))
        )
    else:
        return [Path(file) for file in files]
