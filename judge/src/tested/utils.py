import contextlib
import logging
import os
import random
import shutil
import stat
import string
from os import PathLike
from pathlib import Path

import sys
from typing import IO, List, Union, Generator, TypeVar, Generic, Optional

logger = logging.getLogger(__name__)


def smart_close(file: IO):
    """
    A smart context manager that will close file handles, except the default ones
    (namely stdin, stdout and stderr).
    :param file: The file to close smartly.
    """
    if file and file not in (sys.stdout, sys.stdin, sys.stderr):
        return file

    return contextlib.nullcontext(file)


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


@contextlib.contextmanager
def protected_directory(directory: Union[PathLike, Path]
                        ) -> Generator[Path, None, None]:
    try:
        logger.info("Making %s read-only", directory)
        os.chmod(directory, stat.S_IREAD)  # Disable write access
        yield directory
    finally:
        logger.info("Giving write-access to %s", directory)
        os.chmod(directory, stat.S_IREAD | stat.S_IWRITE)


def basename(file: Union[str, Path]) -> str:
    """
    Get the basename of a file.

    :param file: The file or path.
    :return: The basename.

    >>> basename("test.py")
    'test'
    >>> basename("very/nice/path.java")
    'path'
    """
    if isinstance(file, str):
        file = Path(file)
    return file.stem


T = TypeVar('T')


class Either(Generic[T]):

    def __init__(self, value: Union[T, Exception]):
        self.value = value

    def get(self) -> T:
        if isinstance(self.value, Exception):
            raise self.value
        return self.value

    def maybe(self) -> Optional[T]:
        if isinstance(self.value, Exception):
            return None
        return self.value


def _get_identifier() -> str:
    """Generate a random secret valid in most config."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)
