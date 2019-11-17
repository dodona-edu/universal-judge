"""Evaluators actually compare values to determine the result of a test."""

import math
from dataclasses import field
from typing import Optional, List, Union, Dict, Any

from pydantic.dataclasses import dataclass

import serialisation
from dodona import Status, ExtendedMessage, Permission, StatusMessage, Message
from serialisation import get_readable_representation, NumericTypes, SequenceTypes, SerialisationError
from testplan import SpecificEvaluator as TestplanSpecificEvaluator
from testplan import TestPlanError, TextOutputChannel, FileOutputChannel, ReturnOutputChannel, NoneChannelState, \
    IgnoredChannelState, OutputChannel, AnyChannelState, BuiltinEvaluator, Builtin, CustomEvaluator


@dataclass
class EvaluationResult:
    """Provides the result of an evaluation for a specific output channel."""
    result: StatusMessage  # The result of the evaluation.
    readable_expected: str  # A human-friendly version of what the channel should have been.
    readable_actual: str  # A human-friendly version (best effort at least) of what the channel is.
    messages: List[Message] = field(default_factory=list)


class Evaluator:
    """Base evaluator containing minimal functionality."""

    def __init__(self, arguments: Dict[str, Any] = None):
        self.arguments = arguments or {}

    def evaluate(self, output_channel, actual) -> EvaluationResult:
        """
        Evaluate the result.
        :param output_channel: The output channel, from the testplan.
        :param actual: The actual result, produced by the user's code.
        :return: The result of the evaluation.
        """
        raise NotImplementedError


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

    def evaluate(self, output_channel, actual) -> EvaluationResult:
        assert isinstance(output_channel, TextOutputChannel)

        expected = output_channel.get_data_as_string()

        if not isinstance(actual, str):
            return EvaluationResult(
                result=StatusMessage(enum=Status.WRONG),
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
            result=StatusMessage(enum=Status.CORRECT if result else Status.WRONG),
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

    def evaluate(self, output_channel, actual) -> EvaluationResult:
        assert isinstance(output_channel, FileOutputChannel)
        assert actual is None

        expected = output_channel.expected_path
        actual = output_channel.actual_path

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
                result=StatusMessage(enum=Status.RUNTIME_ERROR, human="Required output file not found."),
                readable_expected=expected,
                readable_actual=actual,
            )

        if self.arguments["mode"] == "exact":
            result = StatusMessage(enum=Status.CORRECT if actual == expected else Status.WRONG)
        elif self.arguments["mode"] == "lines":
            expected_lines = expected.splitlines()
            actual_lines = actual.splitlines()
            correct = len(actual_lines) == len(expected_lines)
            for (expected_line, actual_line) in zip(expected_lines, actual_lines):
                correct = correct and expected_line == actual_line
            result = StatusMessage(enum=Status.CORRECT if correct else Status.WRONG)
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
            result = StatusMessage(enum=Status.CORRECT if correct else Status.WRONG)

        return EvaluationResult(
            result=StatusMessage(enum=Status.CORRECT if result else Status.WRONG),
            readable_expected=expected,
            readable_actual=actual
        )


class ValueEvaluator(Evaluator):
    """
    Evaluate two values. The values must match exact. Currently, this evaluator has no options,
    but it might receive them in the future (e.g. options on how to deal with strings or floats).
    """

    def __init__(self, arguments):
        super().__init__(arguments)

    def evaluate(self, output_channel, actual) -> EvaluationResult:
        assert isinstance(output_channel, ReturnOutputChannel)

        expected = output_channel.value
        readable_expected = get_readable_representation(expected)

        # A crash here indicates a problem with one of the language implementations, or a student
        # is trying to cheat.
        try:
            actual = None if actual is None else serialisation.parse(actual)
            readable_actual = get_readable_representation(actual)
        except SerialisationError as e:
            message = ExtendedMessage(description=str(e), format="text", permission=Permission.STAFF)
            student = "Your return value was wrong; additionally Dodona didn't recognize it. " \
                      "Contact staff for more information."
            return EvaluationResult(
                result=StatusMessage(enum=Status.WRONG, human=student),
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
                result=StatusMessage(enum=Status.WRONG, human="Value is of wrong type."),
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
            result=StatusMessage(enum=Status.CORRECT if correct else Status.WRONG),
            readable_expected=readable_expected,
            readable_actual=readable_actual
        )


class NoneEvaluator(Evaluator):
    """Comparator for no output."""

    def evaluate(self, expected, actual) -> EvaluationResult:
        assert expected == NoneChannelState.NONE
        is_none = actual is None or actual == ""
        return EvaluationResult(
            result=StatusMessage(enum=Status.CORRECT if is_none else Status.WRONG),
            readable_expected="",
            readable_actual="" if is_none else str(actual)
        )


class IgnoredEvaluator(Evaluator):
    """Comparator for ignored output."""

    def evaluate(self, output_channel, actual) -> EvaluationResult:
        assert output_channel == IgnoredChannelState.IGNORED
        return EvaluationResult(
            result=StatusMessage(enum=Status.CORRECT),
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

    def evaluate(self, output_channel, actual) -> EvaluationResult:
        assert output_channel.value is None

        if actual is None:
            return EvaluationResult(
                result=StatusMessage(enum=Status.INTERNAL_ERROR, human="Received no output."),
                readable_expected="",
                readable_actual=""
            )

        try:
            actual: SpecificResult = SpecificResult.__pydantic_model__.parse_raw(actual)
        except (TypeError, ValueError) as e:
            message = ExtendedMessage(description=str(e), format="text", permission=Permission.STAFF)
            student = "Something went wrong while receiving the test result. Contact staff."
            return EvaluationResult(
                result=StatusMessage(enum=Status.INTERNAL_ERROR, human=student),
                readable_expected="",
                readable_actual="",
                messages=[message]
            )

        return EvaluationResult(
            result=StatusMessage(enum=Status.CORRECT if actual.result else Status.WRONG),
            readable_expected=actual.readable_expected,
            readable_actual=actual.readable_actual,
            messages=actual.messages
        )


def get_evaluator(output: Union[OutputChannel, AnyChannelState]) -> Evaluator:
    """
    Get the evaluator for a given output channel.
    """
    # Handle channel states.
    if output == NoneChannelState.NONE:
        return NoneEvaluator()
    if output == IgnoredChannelState.IGNORED:
        return IgnoredEvaluator()

    # Handle actual evaluators.
    evaluator = output.evaluator
    if isinstance(evaluator, BuiltinEvaluator):
        if evaluator.name == Builtin.TEXT:
            return TextEvaluator(arguments=evaluator.options)
        elif evaluator.name == Builtin.FILE:
            return FileEvaluator(arguments=evaluator.options)
        elif evaluator.name == Builtin.VALUE:
            return ValueEvaluator(arguments=evaluator.options)
        else:
            raise AssertionError(f"Unknown built-in enum value: {evaluator.name}")
    elif isinstance(evaluator, CustomEvaluator):
        raise NotImplementedError()
    elif isinstance(evaluator, TestplanSpecificEvaluator):
        return SpecificEvaluator()
    else:
        raise AssertionError(f"Unknown evaluator type: {type(evaluator)}")
