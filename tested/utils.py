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

    for key, value in one.items():
        new_dictionary[key] = value

    # noinspection PyTypeChecker
    for key, value in two.items():
        if isinstance(value, dict) and key in one:
            new_dictionary[key] = recursive_dict_merge(new_dictionary[key], value)
        else:
            new_dictionary[key] = value

    return new_dictionary


def sorting_value_extract(maybe_value: Any) -> Any:
    if hasattr(maybe_value, "data"):
        return maybe_value.data
    else:
        return maybe_value


def sorted_no_duplicates(
    iterable: Iterable[T],
    key: Callable[[T], K] = lambda x: x,
    recursive_key: Callable[[K], K] | None = None,
) -> list[T]:
    from functools import cmp_to_key

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
        # Attempt to use a key to extract the data if needed.
        if recursive_key:  # Parent function parameter
            if x is not None:
                x = recursive_key(x)
            if y is not None:
                y = recursive_key(y)

        # Try to compare types; this might be enough.
        type_compare = type_order(x, y)
        if type_compare != 0:
            return type_compare

        # Next, if we have iterables, attempt to use those (but not for strings)
        # Both should be iterable in this case, since the types are the same.
        if (
            not isinstance(x, str)
            and not isinstance(y, str)
            and isinstance(x, Iterable)
            and isinstance(y, Iterable)
        ):
            return order_iterable(x, y)

        # Finally, attempt to use the values themselves.
        try:
            return int(x < y) - int(x > y)  # type: ignore
        except TypeError:
            # These types cannot be compared, so fallback to string comparison.
            return int(str(x) < str(y)) - int(str(x) > str(y))

    # Use Python's built-in sorted with cmp_to_key for performance.
    # We negate the order result because `order` returns 1 for x < y,
    # but cmp expects -1 for x < y.
    wrapped_key = cmp_to_key(lambda x, y: -order(x, y))

    sorted_iter = sorted(iterable, key=lambda x: wrapped_key(key(x)))

    # Filter out duplicates
    no_dup = []
    if not sorted_iter:
        return no_dup

    last_v = sorted_iter[0]
    last_key_val = key(last_v)
    no_dup.append(last_v)

    for v in sorted_iter[1:]:
        key_v = key(v)
        if order(key_v, last_key_val) != 0:
            no_dup.append(v)
            last_key_val = key_v
            last_v = v

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
