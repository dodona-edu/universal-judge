import contextlib

import sys
from typing import IO


def smart_close(file: IO):
    """
    A smart context manager that will close file handles, except the default ones (namely stdin,
    stdout and stderr).
    :param file: The file to close smartly.
    """
    if file and file not in (sys.stdout, sys.stdin, sys.stderr):
        return file

    return contextlib.nullcontext(file)
