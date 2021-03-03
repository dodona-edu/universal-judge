import contextlib
import logging
import os
import random
import stat
import string
import typing
from decimal import Decimal
from os import PathLike
from itertools import zip_longest
from pathlib import Path
from typing import (IO, Union, Generator, TypeVar, Generic, Optional, Mapping,
                    Iterable, List, Callable, Any)

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
                         key: Optional[Callable[[T], K]] = None,
                         recursive_key: Optional[Callable[[K], K]] = None
                         ) -> List[T]:
    # Identity key function
    def identity(x: T) -> T:
        return x

    # Order functions
    def type_order(x: Any) -> int:
        """
        Determine order for different types
        :param x: value to check
        :return: order index of type
        """
        if x is None:
            return 0
        elif isinstance(x, (int, float, Decimal)):
            return 1
        elif isinstance(x, str):
            return 2
        elif isinstance(x, tuple):
            return 3
        elif isinstance(x, list):
            return 4
        else:
            return 5

    def order_iterable(iter0: Iterable[Any], iter1: Iterable[Any]) -> int:
        """
        Determine order between two iterables

        :param iter0: first iterable
        :param iter1: second iterable
        :return: 1 if iter0 < iter1 else -1 if iter0 > iter1 else 0
        """
        for x, y in zip_longest(iter0, iter1):
            cmp = order(x, y)
            if cmp == 0:
                continue
            else:
                return cmp
        return 0

    def order(x: Any, y: Any) -> int:
        """
        Determine order between two types

        :param x: first value
        :param y: second value
        :return: 1 if x < y else -1 if x > y else 0
        """
        if recursive_key:
            x, y = recursive_key(x), recursive_key(y)
        t_x, t_y = type_order(x), type_order(y)
        if t_x < t_y:
            return 1
        elif t_x > t_y:
            return -1
        elif not isinstance(x, str) and isinstance(x, Iterable):
            return order_iterable(x, y)
        elif x < y:
            return 1
        elif x > y:
            return -1
        else:
            return 0

    # Sort functions, custom implementation needed for efficient recursive ordering
    # of values that have different types
    def insertion_sort(list_t: List[T], start: int, end: int) -> List[T]:
        """
        Insertion sort
        :param list_t: list to sort
        :param start: start of range to sort
        :param end: end of range to sort
        :return: the list itself
        """
        for i in range(start + 1, end):
            j = i - 1
            item = list_t[i]
            item_key = key(item)
            while j >= start and order(key(list_t[j]), item_key) < 0:
                list_t[j + 1] = list_t[j]
                j -= 1
            list_t[j + 1] = item
        return list_t

    def merge(to_list: List[T], from_list: List[T], start: int, middle: int,
              end: int):
        """
        Merge two sorted sublist to sorted list

        :param to_list: output list
        :param from_list: input list
        :param start: start of first half
        :param middle: end of first half, start of second half
        :param end: end of second half
        :return: No return value
        """
        i, j, k = start, middle, start
        while i < middle and j < end:
            if order(key(from_list[i]), key(from_list[j])) > 0:
                to_list[k] = from_list[i]
                i += 1
            else:
                to_list[k] = from_list[j]
                j += 1
            k += 1
        if i < middle:
            to_list[k:end] = from_list[i:middle]
        else:
            to_list[k:end] = from_list[j:end]

    def timsort(list_t: List[T], timgroup: int = 32) -> List[T]:
        """
        Time sort algorithm
        :param list_t: the modifiable list to sort
        :param timgroup: the number of elements to sort with insertion sort
        :return: The sort list
        """
        len_list_t = len(list_t)
        for i in range(0, len_list_t, timgroup):
            insertion_sort(list_t, i, min(i + timgroup, len_list_t))
        copy = list(list_t)
        while timgroup < len_list_t:
            for start in range(0, len_list_t, 2 * timgroup):
                middle = min(len_list_t, start + timgroup)
                end = min(len_list_t, start + 2 * timgroup)
                merge(list_t, copy, start, middle, end)
            list_t, copy = copy, list_t
            timgroup *= 2
        return copy

    # Check if key function is not none
    if key is None:
        key = identity

    # Sort and filterout duplicates
    first, last_key, no_dup, list_iter = True, None, [], list(iterable)
    for v in timsort(list_iter):
        if not first:
            key_v = key(v)
            if order(key_v, last_key) == 0:
                continue
            else:
                no_dup.append(v)
                last_key = key_v
        else:
            first, last_key = False, key(v)
            no_dup.append(v)
    return no_dup
