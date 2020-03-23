import contextlib
import logging
import os
import random
import stat
import string
from os import PathLike
from pathlib import Path
from typing import IO, Union, Generator, TypeVar, Generic, Optional

import sys

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


def get_identifier() -> str:
    """Generate a random secret valid in most configs."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


def consume_shebang(submission: Path) -> Optional[str]:
    """
    Find the shebang in the submission, and if it is present, consume it.

    :param submission: The path to the file containing the code.

    :return: The programming language if found.
    """
    language = None
    with open(submission, "r+") as file:
        lines = file.readlines()
        file.seek(0)

        # Steps to find
        has_potential = True
        for line in lines:
            if has_potential and not line.strip():
                continue
            if stripped := line.strip():
                if has_potential and stripped.startswith("#!tested"):
                    try:
                        _, language = stripped.split(" ")
                    except ValueError:
                        logger.error(f"Invalid shebang on line {stripped}")
                    has_potential = False
                else:
                    file.write(line)
        file.truncate()

    return language
