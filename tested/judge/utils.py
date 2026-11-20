"""
Common utilities for the judge.
"""

import errno
import io
import logging
import os
import shutil
import stat
import subprocess
from pathlib import Path

from attrs import define

from tested import configs
from tested.configs import TESTED_DIRECTORY, Bundle
from tested.languages.language import FileFilter

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


# The image's variables that change how the submission runs; the judge keeps the rest.
STUDENT_ENVIRONMENT = ["PATH", "LANG", "NODE_PATH"]


def _student_environment() -> dict[str, str]:
    return {
        name: os.environ[name] for name in STUDENT_ENVIRONMENT if name in os.environ
    }


# An entry that is a link, gone, a FIFO without writer or out of the judge's reach.
_NOT_OPENED = (errno.ELOOP, errno.ENOENT, errno.ENXIO, errno.EACCES, errno.EPERM)


def _open_up(name: str, directory_fd: int):
    try:
        fd = os.open(
            name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=directory_fd
        )
    except OSError as error:
        if error.errno in _NOT_OPENED:
            return
        raise
    try:
        status = os.fstat(fd)
        if status.st_uid != os.getuid():
            return
        if stat.S_ISDIR(status.st_mode):
            os.fchmod(fd, status.st_mode | 0o777)
        elif stat.S_ISREG(status.st_mode):
            os.fchmod(fd, status.st_mode | 0o666)
    finally:
        os.close(fd)


def _give_student_access(directory: Path):
    """Let student read and write what the judge put in the directory."""
    # fwalk does not enter a directory that became a link after it was listed.
    for _, _, files, directory_fd in os.fwalk(directory, follow_symlinks=False):
        for name in [".", *files]:
            _open_up(name, directory_fd)


def run_command(
    directory: Path,
    timeout: float | None,
    command: list[str] | None = None,
    stdin: str | None = None,
    check: bool = False,
    *,
    as_student: bool,
) -> BaseExecutionResult | None:
    """
    Run a command and get the result of said command.

    :param directory: The directory to execute in.
    :param command: Optional, the command to execute.
    :param stdin: Optional stdin for the process.
    :param timeout: The max time for this command.
    :param check: Raise if the command fails.
    :param as_student: The command compiles or runs the submission, so it runs as
                       the user student when the image supports that.

    :return: The result of the execution if the command was not None.
    """
    if not command:
        return None

    if as_student and configs.runs_as_student():
        _give_student_access(directory)
        command = [str(configs.RUN_AS_STUDENT), *command]
        environment = _student_environment()
        # Leaves what the student creates readable and writable for the judge.
        umask = 0
        # Not the judge's own stdin.
        standard_input = subprocess.DEVNULL if stdin is None else None
    else:
        environment = None
        umask = -1
        standard_input = None

    try:
        process = subprocess.run(
            command,
            cwd=directory,
            env=environment,
            umask=umask,
            text=True,
            errors="backslashreplace",
            capture_output=True,
            input=stdin,
            stdin=standard_input,
            timeout=timeout,
            check=check,
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
        memory=process.returncode == -9,
    )


def read_submission_file(path: Path) -> str:
    """Read a file the submission's process wrote, as the user that process ran as."""
    if not configs.runs_as_student():
        with open(path, "r") as file:
            return file.read()

    # lstat: only student's cat may follow a link.
    if stat.S_ISDIR(path.lstat().st_mode):
        raise IsADirectoryError(errno.EISDIR, os.strerror(errno.EISDIR), str(path))
    process = subprocess.run(
        [str(configs.RUN_AS_STUDENT), "cat", "--", str(path)],
        capture_output=True,
        env=_student_environment(),
        check=False,
    )
    if process.returncode != 0:
        error = process.stderr.decode(errors="backslashreplace")
        raise OSError(f"student cannot read {path}: {error}")
    # Decodes like open(path, "r").
    return io.TextIOWrapper(io.BytesIO(process.stdout)).read()


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
                f"Could not find dependency file {file}, looked in {origins}"
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
            file = origin.name
            if origin.is_file() and (
                all_files or bundle.language.is_source_file(origin)
            ):
                source_files.append(str(dst / origin.name))
                _logger.debug("Copying %s to %s", origin, dst)
                shutil.copy2(origin, dst)
            elif origin.is_dir() and file != TESTED_DIRECTORY:
                _logger.debug("Iterate subdir %s", dst / file)
                shutil.copytree(origin, dst / file)

    recursive_copy(bundle.config.workdir, destination)

    return source_files


def filter_files(files: list[str] | FileFilter, directory: Path) -> list[Path]:
    if callable(files):
        return [x.relative_to(directory) for x in filter(files, directory.rglob("*"))]
    else:
        return [Path(file) for file in files]
