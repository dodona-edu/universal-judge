import contextlib
import sys

from typing import IO


@contextlib.contextmanager
def smart_close(file: IO) -> IO:
    """
    A smart context manager that will close file handles, except the default ones (namely stdin,
    stdout and stderr).
    :param file: The file to close smartly.
    """
    try:
        yield file
    finally:
        if file and file not in (sys.stdout, sys.stdin, sys.stderr):
            print("Closing the file, not a default stream.")
            file.close()
