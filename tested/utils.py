import contextlib
import logging
import os
import random
import stat
import string
import typing
from decimal import Decimal
from os import PathLike
from pathlib import Path
from typing import (IO, Union, Generator, TypeVar, Generic, Optional, Mapping,
                    Iterable, List, Callable)

import itertools
import sys

_logger = logging.getLogger(__name__)


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
        _logger.info("Making %s read-only", directory)
        os.chmod(directory, stat.S_IREAD)  # Disable write access
        yield directory
    finally:
        _logger.info("Giving write-access to %s", directory)
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
    try:
        # noinspection PyTypeChecker
        with open(submission, "r+") as file:
            lines = file.readlines()
            file.seek(0)

            # Steps to find
            has_potential = True
            for line in lines:
                stripped = line.strip()
                if has_potential and stripped.startswith("#!tested"):
                    try:
                        _, language = stripped.split(" ")
                    except ValueError:
                        _logger.error(f"Invalid shebang on line {stripped}")
                else:
                    file.write(line)
                if has_potential and stripped:
                    has_potential = False
            file.truncate()
    except FileNotFoundError:
        pass

    return language


K = TypeVar('K')
V = TypeVar('V')


class _FallbackDict(dict, Generic[K, V]):

    def __init__(self, existing: Mapping[K, V], fallback_: Mapping[K, V]):
        super().__init__(existing)
        self.fallback = fallback_

    def __missing__(self, key: K) -> Optional[V]:
        return self.fallback[key]


def fallback(source: Mapping[K, V], additions: Mapping[K, V]) -> Mapping[K, V]:
    return _FallbackDict(additions, source)


def get_args(type_):
    """
    Get the args of a type or the type itself.

    This function is basically the same as `typing.get_args`, but it will return
    the type itself if the typing function returns nothing.

    Use of this function allows uniform isinstance checks, regardless of how many
    parameters a Union has or even regardless if the type is a generic or not.

    Some examples:

    >>> import typing
    >>> Test = typing.Union[str]
    >>> Test2 = typing.Union[str, int]
    >>> a = "hallo"
    >>> isinstance(a, typing.get_args(Test))
    False
    >>> isinstance(a, typing.get_args(Test2))
    True
    >>> isinstance(a, get_args(Test))
    True
    >>> isinstance(a, get_args(Test2))
    True
    >>> isinstance(a, get_args(str))
    True
    >>> isinstance(a, typing.get_args(str))
    False

    :param type_: The type to resolve.
    :return: The resolved generics or the type itself.
    """
    if a := typing.get_args(type_):
        return a
    else:
        return type_,


def flatten(nested: Iterable[Iterable[T]]) -> Iterable[T]:
    """
    Flatten a list of lists one level.

    >>> flatten([[0], [1], [2]])
    [0, 1, 2]
    """
    return filter(None, itertools.chain.from_iterable(nested))


def camelize(what: str) -> str:
    """
    Convert a string to camelCase from snake_case. The algorithm is simple: each
    underscore is removed and the letter behind it will be capitalized. The first
    letter will be downcased.

    >>> camelize("__foo_bar__")
    'fooBar'
    >>> camelize("this_is_snake_case")
    'thisIsSnakeCase'
    >>> camelize("_Weird_cases_aRe_mostly_KEPT")
    'weirdCasesAReMostlyKEPT'
    >>> camelize("numbers_1_2_are_not_special")
    'numbers12AreNotSpecial'
    >>> camelize("________________")
    ''

    :param what: The string to convert.
    :return: The converted string.
    """
    result = pascalize(what)
    return (result[0].lower() + result[1:]) if result else ""


def pascalize(what: str) -> str:
    """
    Convert a string to PascalCase from snake_case.

    >>> pascalize("__foo_bar__")
    'FooBar'
    >>> pascalize("this_is_snake_case")
    'ThisIsSnakeCase'
    >>> pascalize("_Weird_cases_aRe_mostly_KEPT")
    'WeirdCasesAReMostlyKEPT'
    >>> pascalize("numbers_1_2_are_not_special")
    'Numbers12AreNotSpecial'
    >>> pascalize("________________")
    ''

    :param what: The string to convert.
    :return: The converted string.
    """
    result = ""
    i = 0
    while i < len(what):
        this = what[i]
        if this == "_":
            i += 1
            if i < len(what):
                while i < len(what) - 1 and what[i] == "_":
                    i += 1
                r = what[i]
                if r != "_":
                    result += r.upper()
        else:
            result += this
        i += 1
    return (result[0].upper() + result[1:]) if result else ""


def snake_case(what: str) -> str:
    """
    Emits a warning if the string is not in snake_case. The check is simple: it
    just checks for capitals.

    :param what: The name.
    :return: The same name.
    """
    if any(x.isupper() for x in what):
        _logger.warning(
            f"A name '{what}' is not in snake_case. This might cause problems."
        )
    return what


def safe_del(l: List[T], index: int, f: Callable[[T], bool]) -> bool:
    """
    Delete an item from a list at a position if the filter is True. If the index
    is out of ranger or the filter is False, the function will return False, else
    True.

    :param l: The list to delete from.
    :param index: The index to delete at.
    :param f: The filter for the element.

    :return: True or False
    """
    try:
        v = l[index]
        if f(v):
            del l[index]
            return True
        else:
            return False
    except IndexError:
        return False


def safe_get(l: List[T], index: int) -> Optional[T]:
    """
    Get the element at the given position or None if the index is out of bounds.
    """
    try:
        return l[index]
    except IndexError:
        return None


def sorted_no_duplicates(iterable: Iterable[T],
                         key: Optional[Callable[[T], K]] = None) -> List[T]:
    def identity(x: T) -> T:
        return x

    def any_sortable(x: K) -> typing.Tuple[bool, bool, bool, bool, bool, K]:
        return (
            x is not None,  # First all None, then bool
            not isinstance(x, (int, float, Decimal)),  # Then numbers
            not isinstance(x, str),  # Then strings
            not isinstance(x, tuple),  # Then tuples
            not isinstance(x, list),  # Then lists
            x  # Original sort value
        )

    if key is None:
        key = identity

    def _key(x):
        return any_sortable(key(x))

    first, key_last, no_dup = True, None, []
    for v in sorted(iterable, key=_key):
        if not first:
            key_v = _key(v)
            if key_v == key_last:
                continue
            else:
                no_dup.append(v)
                key_last = key_v
        else:
            first = False
            no_dup.append(v)
    return no_dup
