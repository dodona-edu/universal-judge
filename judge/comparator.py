import math
from typing import Optional, Union, Tuple, Any

from testplan import TestPlanError, Value, ValueType


class Comparator:

    def evaluate(self, expected, actual) -> bool:
        raise NotImplementedError

    def get_readable_input(self, expected) -> str:
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

    def evaluate(self, expected, actual) -> bool:
        if self.ignore_whitespace:
            expected = expected.strip()
            actual = actual.strip()
        if self.case_insensitive:
            expected = expected.lower()
            actual = actual.lower()

        actual_float = _is_number(actual)
        if self.allow_floating_point and actual_float:
            expected_float = _is_number(expected)
            return expected_float and math.isclose(actual_float, expected_float)
        else:
            return actual == expected

    def get_readable_input(self, expected) -> str:
        if self.ignore_whitespace:
            expected = expected.strip()
        if self.case_insensitive:
            expected = expected.lower()
        return expected


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

    def evaluate(self, expected, actual) -> bool:
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

        return actual == expected

    def get_readable_input(self, expected) -> str:
        return expected


def _definition_to_type(definition: Union[dict, Value]) -> Tuple[Any, ValueType]:
    if isinstance(definition, dict):
        definition = Value.from_dict(definition)
    if definition.type == ValueType.integer:
        return int(definition.data), ValueType.integer
    elif definition.type == ValueType.rational:
        return float(definition.data), ValueType.rational
    elif definition.type == ValueType.text:
        return definition.data, ValueType.text
    elif definition.type == ValueType.boolean:
        return bool(definition.data), ValueType.boolean
    else:
        raise TestPlanError(f"Unknown data type {definition.type} for {definition}.")


class ValueComparator(Comparator):

    # TODO: allow specification what needs to happen per type.
    def __init__(self, arguments=None):
        if not arguments:
            arguments = set()
        self.ignore_whitespace = "ignore_whitespace" in arguments
        self.case_insensitive = "case_insensitive" in arguments

    def evaluate(self, expected, actual) -> bool:
        expected_v, expected_t = None if expected is None else _definition_to_type(expected)
        actual_v, actual_t = None if actual is None else _definition_to_type(actual)
        return expected_v == actual_v and expected_t == actual_t

    def get_readable_input(self, expected) -> str:
        return _definition_to_type(expected)[0]
