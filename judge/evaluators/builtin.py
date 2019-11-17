"""Built-in evaluators."""

import math
from dataclasses import field
from typing import Optional, List

from pydantic.dataclasses import dataclass

import serialisation
from dodona.common import Status, ExtendedMessage, Permission
from dodona.partial_output import StatusMessage
from evaluators.common import Evaluator, EvaluationResult
from testplan import TestPlanError
from serialisation import get_readable_representation, NumericTypes, SequenceTypes, SerialisationError


def _is_number(string: str) -> Optional[float]:
    try:
        return float(string)
    except ValueError:
        return None


class TextEvaluator(Evaluator):
    """
    Basic evaluator that will evaluate two texts. As this evaluator is intended for evaluating
    stdout, it supports various options to make life easier:

    - ``ignoreWhitespace``: whitespace before and after will be stripped
    - ``caseInsensitive``: all comparisons will be in lower-case
    - ``tryFloatingPoint``: try to evaluate the value as a floating point
    - ``applyRounding``: limit floating points to ``roundTo`` numbers
    - ``roundTo``: amount of numbers to round to.

    Note: floating points inside other texts are currently not supported.
    """

    def __init__(self, arguments):
        super().__init__(arguments)
        self.arguments.update({
            # Options for textual comparison
            'ignoreWhitespace': True,
            'caseInsensitive': False,
            # Options for numerical comparison
            'tryFloatingPoint': False,
            'applyRounding': False,
            'roundTo': 3
        })

    def evaluate(self, expected, actual) -> EvaluationResult:
        if not isinstance(expected, str):
            raise ValueError("Expected value must be string.")
        if not isinstance(actual, str):
            return EvaluationResult(
                result=StatusMessage(Status.WRONG),
                readable_expected=expected,
                readable_actual=""
            )

        if self.arguments['ignoreWhitespace']:
            expected = expected.strip()
            actual = actual.strip()

        if self.arguments['caseInsensitive']:
            expected = expected.lower()
            actual = actual.lower()

        if self.arguments['tryFloatingPoint'] and (actual_float := _is_number(actual)):
            expected_float = float(expected)
            if self.arguments['applyRounding']:
                numbers = int(self.arguments['roundTo'])
                # noinspection PyUnboundLocalVariable
                actual_float = round(actual_float, numbers)
                expected_float = round(expected_float, numbers)
            # noinspection PyUnboundLocalVariable
            result = math.isclose(actual_float, expected_float)
        else:
            result = actual == expected

        return EvaluationResult(
            result=StatusMessage(Status.CORRECT if result else Status.WRONG),
            readable_expected=str(expected),
            readable_actual=str(actual)
        )


class FileEvaluator(Evaluator):
    """
    Evaluate the contents of two files. The file evaluator supports one option, ``mode``, used to
    define in which mode the evaluator should operate:

    1. ``exact``: Both files must be exactly the same, including line separators.
    2. ``lines``: Each line is evaluated against the corresponding line in the other file. The
                 evaluation itself is exact.
    3. ``values``: Each line is evaluated as a textual value, using the :class:`TextEvaluator`.
                   In this mode, options from this evaluator are passed to the text evaluator.

    When no mode is passed, the evaluator will default to exact.
    """

    def __init__(self, arguments):
        super().__init__(arguments)
        self.arguments.update({
            "mode": "exact"
        })
        if self.arguments["mode"] not in {"exact", "lines", "values"}:
            raise TestPlanError(f"Unknown mode for file evaluator: {self.arguments['mode']}")

    def evaluate(self, expected, actual) -> EvaluationResult:
        try:
            with open(expected, "r") as file:
                expected = file.read()
        except FileNotFoundError:
            raise TestPlanError(f"File containing expected data {expected} not found.")

        try:
            with open(actual, "r") as file:
                actual = file.read()
        except FileNotFoundError:
            return EvaluationResult(
                result=StatusMessage(Status.RUNTIME_ERROR, "Required output file not found."),
                readable_expected=expected,
                readable_actual=actual,
            )

        if self.arguments["mode"] == "exact":
            result = StatusMessage(Status.CORRECT if actual == expected else Status.WRONG)
        elif self.arguments["mode"] == "lines":
            expected_lines = expected.splitlines()
            actual_lines = actual.splitlines()
            correct = len(actual_lines) == len(expected_lines)
            for (expected_line, actual_line) in zip(expected_lines, actual_lines):
                correct = correct and expected_line == actual_line
            result = StatusMessage(Status.CORRECT if correct else Status.WRONG)
        else:
            assert self.arguments["mode"] == "values"
            text_evaluator = TextEvaluator(self.arguments)
            expected_lines = expected.splitlines()
            actual_lines = actual.splitlines()
            correct = len(actual_lines) == len(expected_lines)
            # Overwrite the expected and actual with the values from the text evaluator.
            # This will give a more consistent output in Dodona.
            expected = []
            actual = []
            for (expected_line, actual_line) in zip(expected_lines, actual_lines):
                r = text_evaluator.evaluate(expected_line, actual_line)
                expected.append(r.readable_expected)
                actual.append(r.readable_actual)
                correct = correct and r.result.enum == Status.CORRECT
            result = StatusMessage(Status.CORRECT if correct else Status.WRONG)

        return EvaluationResult(
            result=StatusMessage(Status.CORRECT if result else Status.WRONG),
            readable_expected=expected,
            readable_actual=actual
        )


class ValueComparator(Evaluator):
    """
    Evaluate two values. The values must match exact. Currently, this evaluator has no options,
    but it might receive them in the future (e.g. options on how to deal with strings or floats).
    """

    def __init__(self, arguments):
        super().__init__(arguments)

    def evaluate(self, expected, actual) -> EvaluationResult:
        # A crash here indicates a problem with the testplan.
        try:
            expected = None if expected is None else serialisation.parse(expected)
            readable_expected = get_readable_representation(expected)
        except SerialisationError as e:
            message = ExtendedMessage(str(e), "text", Permission.STAFF)
            return EvaluationResult(
                result=StatusMessage(Status.INTERNAL_ERROR, "There is an error with this exercise."),
                readable_expected=str(expected),
                readable_actual=str(actual),
                messages=[message]
            )

        # A crash here indicates a problem with one of the language implementations, or a student
        # is trying to cheat.
        try:
            actual = None if actual is None else serialisation.parse(actual)
            readable_actual = get_readable_representation(actual)
        except SerialisationError as e:
            message = ExtendedMessage(str(e), "text", Permission.STAFF)
            student = "Your return value was wrong; additionally Dodona didn't recognize it. " \
                      "Contact staff for more information."
            return EvaluationResult(
                result=StatusMessage(Status.WRONG, student),
                readable_expected=readable_expected,
                readable_actual=str(actual),
                messages=[message]
            )

        # Two types are valid if
        # 1. They are both None
        # 2. They have the same type
        if not (expected is None and actual is None) and (
                expected is None or actual is None or expected.type != actual.type):
            return EvaluationResult(
                result=StatusMessage(Status.WRONG, "Value is of wrong type."),
                readable_expected=readable_expected,
                readable_actual=readable_actual
            )

        if expected.data == NumericTypes.REAL:
            correct = math.isclose(expected.data, actual.data)
        elif expected.data == SequenceTypes.LIST:
            correct = list(expected.data) == list(actual.data)
        elif expected.data == SequenceTypes.SET:
            correct = set(expected.data) == set(actual.data)
        else:
            correct = expected.data == actual.data

        return EvaluationResult(
            result=StatusMessage(Status.CORRECT if correct else Status.WRONG),
            readable_expected=readable_expected,
            readable_actual=readable_actual
        )


class NoComparator(Evaluator):
    """Comparator for no output."""

    def evaluate(self, expected, actual) -> EvaluationResult:
        assert expected is None
        return EvaluationResult(
            result=StatusMessage(Status.CORRECT if actual is None else Status.WRONG),
            readable_expected="",
            readable_actual="" if actual is None else str(actual)
        )


@dataclass
class SpecificResult:
    """Result of an evaluation by a language specific evaluator."""
    result: bool  # The result of the evaluation.
    readable_expected: str  # A human-friendly version of what the channel should have been.
    readable_actual: str  # A human-friendly version (best effort at least) of what the channel is.
    messages: List[str] = field(default_factory=list)


class SpecificEvaluator(Evaluator):
    """
    Compare result of custom evaluation code. This evaluator has no options.
    """

    def evaluate(self, expected, actual) -> EvaluationResult:
        assert expected is None

        if actual is None:
            return EvaluationResult(
                result=StatusMessage(Status.INTERNAL_ERROR, "Received no output."),
                readable_expected="",
                readable_actual=""
            )

        try:
            actual: SpecificResult = SpecificResult.__pydantic_model__.parse_raw(actual)
        except (TypeError, ValueError) as e:
            message = ExtendedMessage(str(e), "text", Permission.STAFF)
            student = "Something went wrong while receiving the test result. Contact staff."
            return EvaluationResult(
                result=StatusMessage(Status.INTERNAL_ERROR, student),
                readable_expected="",
                readable_actual="",
                messages=[message]
            )

        return EvaluationResult(
            result=StatusMessage(Status.CORRECT if actual.result else Status.WRONG),
            readable_expected=actual.readable_expected,
            readable_actual=actual.readable_actual,
            messages=actual.messages
        )
