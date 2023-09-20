import contextlib
import itertools
import logging
import random
import string
import sys
from collections.abc import Callable, Iterable
from itertools import zip_longest
from pathlib import Path
from typing import IO, TYPE_CHECKING, Any, TypeGuard, TypeVar
from typing import get_args as typing_get_args

if TYPE_CHECKING:
    from tested.serialisation import Assignment

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


def basename(file: str | Path) -> str:
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


def get_identifier() -> str:
    """Generate a random secret valid in most configs."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + "".join(rest)


K = TypeVar("K")
T = TypeVar("T")


def get_args(type_: Any) -> tuple[Any, ...]:
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
    if a := typing_get_args(type_):
        return a
    else:
        return (type_,)


def flatten(nested: Iterable[Iterable[T]]) -> Iterable[T]:
    """
    Flatten a list of lists one level.

    >>> list(flatten([[0], [1], [2]]))
    [0, 1, 2]
    """
    return itertools.chain.from_iterable(nested)


def safe_del(l: list[T], index: int, f: Callable[[T], bool]) -> bool:
    """
    Delete an item from a list at a position if the filter is True. If the index
    is out of range or the filter is False, the function will return False, else
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


def safe_get(l: list[T], index: int) -> T | None:
    """
    Get the element at the given position or None if the index is out of bounds.
    """
    try:
        return l[index]
    except IndexError:
        return None


def recursive_dict_merge(one: dict, two: dict) -> dict:
    """
    Recursively merge dictionaries, i.e. keys that are dictionaries are merged
    instead of overridden.

    :param one: Dictionary A.
    :param two: Dictionary B.
    :return: A new, merged dictionary.
    """
    new_dictionary = {}

    # noinspection PyTypeChecker
    for key, value in one.items():
        new_dictionary[key] = value

    # noinspection PyTypeChecker
    for key, value in two.items():
        if isinstance(value, dict) and key in one:
            new_dictionary[key] = recursive_dict_merge(new_dictionary[key], value)
        else:
            new_dictionary[key] = value

    return new_dictionary


def sorted_no_duplicates(
    iterable: Iterable[T],
    key: Callable[[T], K] = lambda x: x,
    recursive_key: Callable[[K], K] | None = None,
) -> list[T]:
    # Order functions
    def type_order(x: Any, y: Any) -> int:
        """
        Determine order for different types
        :param x: value one
        :param y: value two
        :return: order index of type
        """
        x, y = str(type(x)), str(type(y))
        return int(x < y) - int(x > y)

    def order_iterable(iter0: Iterable[Any], iter1: Iterable[Any]) -> int:
        """
        Determine order between two iterables

        :param iter0: first iterable
        :param iter1: second iterable
        :return: 1 if iter0 < iter1 else -1 if iter0 > iter1 else 0
        """
        for x, y in zip_longest(iter0, iter1):
            cmp = order(x, y)
            if cmp != 0:
                return cmp
        return 0

    def order(x: Any, y: Any) -> int:
        """
        Determine order between two types

        :param x: first value
        :param y: second value
        :return: 1 if x < y else -1 if x > y else 0
        """
        if recursive_key:  # Parent function parameter
            if x is not None:
                x = recursive_key(x)
            if y is not None:
                y = recursive_key(y)
        cmp = type_order(x, y)
        if cmp != 0:
            return cmp
        elif not isinstance(x, str) and isinstance(x, Iterable):
            return order_iterable(x, y)
        else:
            return int(x < y) - int(x > y)

    # Sort functions, custom implementation needed for efficient recursive ordering
    # of values that have different types
    def insertion_sort(list_t: list[T], start: int, end: int) -> list[T]:
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

    def merge(to_list: list[T], from_list: list[T], start: int, middle: int, end: int):
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

    def timsort(list_t: list[T], timgroup: int = 32) -> list[T]:
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


def is_statement_strict(statement: Any) -> TypeGuard["Assignment"]:
    """
    Check that the given value is a strict statement: it must be a statement but
    not an expression.

    :param statement: The potential statement to check.
    :return: True if it is, False otherwise.
    """
    from tested.serialisation import Assignment

    return isinstance(statement, Assignment)
