import math
from typing import Optional, Tuple

from testplan import TestPlanError


class Comparator:

    def evaluate(self, expected: str, actual: str) -> Tuple[bool, str]:
        raise NotImplementedError


def _is_number(string: str) -> Optional[float]:
    try:
        return float(string)
    except ValueError:
        return None


class TextComparator(Comparator):
    """
    Compares text and optionally floating point numbers. By default, the comparator will match exact tests.
    A series of flags can control the output:
    """

    def __init__(self, arguments=None):

        if not arguments:
            arguments = {"ignoreWhitespace"}

        self.ignore_whitespace = "ignoreWhitespace" in arguments
        self.allow_floating_point = "allowFloating_point" in arguments
        self.case_insensitive = "caseInsensitive" in arguments

    def evaluate(self, expected: str, actual: str) -> Tuple[bool, str]:
        if self.ignore_whitespace:
            expected = expected.strip()
            actual = actual.strip()
        if self.case_insensitive:
            expected = expected.lower()
            actual = actual.lower()

        actual_float = _is_number(actual)
        if self.allow_floating_point and actual_float:
            expected_float = _is_number(expected)
            return expected_float and math.isclose(actual_float, expected_float), str(expected_float)
        else:
            return actual == expected, expected


class FileComparator(Comparator):
    """
    Compares text and optionally floating point numbers. By default, the comparator will match exact tests.
    A series of flags can control the output:
    """

    def __init__(self, arguments=None):

        if not arguments:
            arguments = set()
        self.ignore_whitespace = "ignore_whitespace" in arguments
        self.case_insensitive = "case_insensitive" in arguments

    def evaluate(self, expected: str, actual: str) -> Tuple[bool, str]:
        try:
            with open(expected, "r") as file:
                expected = file.read()
        except FileNotFoundError:
            raise TestPlanError(f"File containing expected data {expected} not found.")

        with open(actual, "r") as file:
            actual = file.read()

        if self.ignore_whitespace:
            expected = expected.strip()
            actual = actual.strip()
        if self.case_insensitive:
            expected = expected.lower()
            actual = actual.lower()

        return actual == expected, expected
