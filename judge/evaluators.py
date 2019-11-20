"""Evaluators actually compare values to determine the result of a test."""

import math
from dataclasses import field
from typing import Optional, List, Union, Dict, Any, Tuple

from pydantic.dataclasses import dataclass

import serialisation
from dodona import Status, ExtendedMessage, Permission, StatusMessage, Message
from runners.common import get_runner
from serialisation import get_readable_representation, SerialisationError
from tested import Config
from testplan import CustomEvaluator as TestplanCustomEvaluator
from testplan import SpecificEvaluator as TestplanSpecificEvaluator
from testplan import TestPlanError, TextOutputChannel, FileOutputChannel, ReturnOutputChannel, NoneChannelState, \
    IgnoredChannelState, OutputChannel, AnyChannelState, BuiltinEvaluator, Builtin


@dataclass
class EvaluationResult:
    """Provides the result of an evaluation for a specific output channel."""
    result: StatusMessage  # The result of the evaluation.
    readable_expected: str  # A human-friendly version of what the channel should have been.
    readable_actual: str  # A human-friendly version (on a best-efforts basis) of what the channel is.
    messages: List[Message] = field(default_factory=list)


class Evaluator:
    """Base evaluator containing minimal functionality."""

    def __init__(self, config: Config, arguments: Dict[str, Any] = None):
        self.config = config
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
    The base evaluator, used to compare two strings. As this evaluator is intended for evaluating
    stdout, it supports various options to make life easier:

    - ``ignoreWhitespace``: whitespace before and after will be stripped
    - ``caseInsensitive``: all comparisons will be in lower-case
    - ``tryFloatingPoint``: try to evaluate the value as a floating-point
    - ``applyRounding``: limit floating points to ``roundTo`` numbers
    - ``roundTo``: amount of numbers to round to.

    Note: floating points inside other texts are currently not supported.
    """

    def __init__(self, config, arguments):
        super().__init__(config, arguments)
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

        expected = output_channel.get_data_as_string(self.config.resources)

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

    def __init__(self, config, arguments):
        super().__init__(config, arguments)
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
            text_evaluator = TextEvaluator(self.config, self.arguments)
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


def _get_values(output_channel, actual) \
        -> Union[EvaluationResult, Tuple[serialisation.Value, str, serialisation.Value, str]]:
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

    return expected, readable_expected, actual, readable_actual


class ValueEvaluator(Evaluator):
    """
    Evaluate two values. The values must match exact. Currently, this evaluator has no options,
    but it might receive them in the future (e.g. options on how to deal with strings or floats).
    """

    def evaluate(self, output_channel, actual) -> EvaluationResult:
        assert isinstance(output_channel, ReturnOutputChannel)

        result = _get_values(output_channel, actual)
        if isinstance(result, EvaluationResult):
            return result
        else:
            expected, readable_expected, actual, readable_actual = result

        # If one of the types is None, but not both, this is not correct.
        if (expected is None and actual is not None) or (expected is not None and actual is None):
            return EvaluationResult(
                result=StatusMessage(enum=Status.WRONG, human="One of the values is nothing."),
                readable_expected=readable_expected,
                readable_actual=readable_actual
            )

        expected = serialisation.to_python_comparable(expected)
        actual = serialisation.to_python_comparable(actual)

        correct = expected == actual

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
    Compare the result of a specific evaluator. This evaluator has no options.
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


@dataclass
class CustomResult:
    """Result of an evaluation by a language specific evaluator."""
    result: bool  # The result of the evaluation.
    messages: List[str] = field(default_factory=list)


class CustomEvaluator(Evaluator):
    """
    Compare the result of a custom evaluator.  This evaluator has no options.
    """

    def evaluate(self, output_channel, actual) -> EvaluationResult:
        assert isinstance(output_channel.evaluator, TestplanCustomEvaluator)
        # In all cases except when dealing with a return value, we manually convert
        # the string to a Value, since that makes our life much easier later on.
        if isinstance(output_channel, TextOutputChannel):
            expected = serialisation.StringType(
                serialisation.StringTypes.TEXT,
                output_channel.get_data_as_string(self.config.resources)
            )
            readable_expected = expected
            actual = serialisation.StringType(
                type=serialisation.StringTypes.TEXT,
                data=actual
            )
            readable_actual = actual
        elif isinstance(output_channel, FileOutputChannel):
            assert actual is None
            expected = serialisation.StringType(
                type=serialisation.StringTypes.TEXT,
                data=output_channel.expected_path
            )
            readable_expected = expected
            actual = serialisation.StringType(
                type=serialisation.StringTypes.TEXT,
                data=output_channel.actual_path
            )
            readable_actual = actual
        elif isinstance(output_channel, ReturnOutputChannel):
            assert output_channel.value is not None
            result = _get_values(output_channel, actual)
            if isinstance(result, EvaluationResult):
                return result
            else:
                expected, readable_expected, actual, readable_actual = result
        else:
            raise AssertionError(f"Unknown type of output channel: {output_channel}")

        runner = get_runner(self.config, output_channel.evaluator.language)
        code = output_channel.evaluator.code.get_data_as_string(self.config.resources)
        result = runner.evaluate_specific(code, expected, actual)

        if not result.stdout:
            message = ExtendedMessage(description=result.stdout, format="text")
            student = "An error occurred while evaluating your exercise."
            return EvaluationResult(
                result=StatusMessage(enum=Status.WRONG, human=student),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=[message]
            )

        try:
            evaluation_result: CustomResult = CustomResult.__pydantic_model__.parse_raw(result.stdout)
        except (TypeError, ValueError) as e:
            message = ExtendedMessage(description=str(e), format="text", permission=Permission.STAFF)
            student = "Something went wrong while receiving the test result. Contact staff."
            return EvaluationResult(
                result=StatusMessage(enum=Status.INTERNAL_ERROR, human=student),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=[message]
            )

        return EvaluationResult(
            result=StatusMessage(enum=Status.CORRECT if evaluation_result.result else Status.WRONG),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
            messages=evaluation_result.messages
        )


def get_evaluator(config: Config, output: Union[OutputChannel, AnyChannelState]) -> Evaluator:
    """
    Get the evaluator for a given output channel.
    """
    # Handle channel states.
    if output == NoneChannelState.NONE:
        return NoneEvaluator(config)
    if output == IgnoredChannelState.IGNORED:
        return IgnoredEvaluator(config)

    # Handle actual evaluators.
    evaluator = output.evaluator
    if isinstance(evaluator, BuiltinEvaluator):
        if evaluator.name == Builtin.TEXT:
            return TextEvaluator(config, arguments=evaluator.options)
        elif evaluator.name == Builtin.FILE:
            return FileEvaluator(config, arguments=evaluator.options)
        elif evaluator.name == Builtin.VALUE:
            return ValueEvaluator(config, arguments=evaluator.options)
        else:
            raise AssertionError(f"Unknown built-in enum value: {evaluator.name}")
    elif isinstance(evaluator, TestplanCustomEvaluator):
        return CustomEvaluator(config)
    elif isinstance(evaluator, TestplanSpecificEvaluator):
        return SpecificEvaluator(config)
    else:
        raise AssertionError(f"Unknown evaluator type: {type(evaluator)}")
