"""
Programmed evaluator.
"""
from typing import Tuple, Optional

from . import EvaluationResult, EvaluatorConfig, value
from .value import get_values
from ..datatypes import StringTypes
from ..dodona import ExtendedMessage, StatusMessage, Status, Permission
from ..judge import evaluate_programmed
from ..languages.generator import convert_expression
from ..serialisation import StringType, SpecificResult, Value
from ..testplan import (TextOutputChannel, FileOutputChannel, ValueOutputChannel,
                        NormalOutputChannel, ExceptionOutputChannel,
                        ProgrammedEvaluator)
from ..utils import Either, get_args

DEFAULT_STUDENT = ("Er ging iets fout op bij het evalueren van de oplossing. Meld "
                   "dit aan de lesgever!")


def _maybe_string(value_: str) -> Optional[Value]:
    if value_:
        return StringType(StringTypes.TEXT, value_)
    else:
        return None


def _try_specific(value_: str) -> SpecificResult:
    return SpecificResult.__pydantic_model__.parse_raw(value_)


def expected_as_value(config: EvaluatorConfig,
                      channel: NormalOutputChannel,
                      actual: str) -> Tuple[Optional[Value], Either[Value]]:
    if isinstance(channel, TextOutputChannel):
        expected = channel.get_data_as_string(config.bundle.config.resources)
        expected_value = _maybe_string(expected)
        actual_value = StringType(StringTypes.TEXT, expected)
        return expected_value, Either(actual_value)

    if isinstance(channel, FileOutputChannel):
        expected = _maybe_string(channel.expected_path)
        actual = StringType(
            type=StringTypes.TEXT,
            data=channel.actual_path
        )
        return expected, Either(actual)

    if isinstance(channel, ValueOutputChannel):
        expected = channel.value
        try:
            actual = value.try_as_value(actual).get()
        except (ValueError, TypeError) as e:
            return expected, Either(e)
        return expected, Either(actual)

    if isinstance(channel, ExceptionOutputChannel):
        raise AssertionError("Programmed evaluation is not support for exceptions.")

    raise AssertionError(f"Unknown channel type for {channel}.")


def evaluate(config: EvaluatorConfig,
             channel: NormalOutputChannel,
             actual: str,
             wrong: Status, timeout: Optional[float]) -> EvaluationResult:
    """
    Evaluate using a programmed evaluator. This evaluator is unique, in that it is
    also responsible for running the evaluator (all other evaluators don't do that).
    """
    assert isinstance(channel, get_args(NormalOutputChannel))
    assert hasattr(channel, 'evaluator')
    assert isinstance(channel.evaluator, ProgrammedEvaluator)

    # Convert the expected item to a Value, which is then passed to the
    # evaluator for evaluation.
    # This is slightly tricky, since the actual value must also be converted
    # to a value, and we are not yet sure what the actual value is exactly
    expected_value, actual_value = get_values(config.bundle, channel, actual or "")
    readable_expected = convert_expression(config.bundle, expected_value)

    try:
        actual_value = actual_value.get()
    except (ValueError, TypeError) as e:
        staff_message = ExtendedMessage(
            description=f"An error occurred while receiving output for programmed "
                        f"evaluation. Error was {e}",
            format="text",
            permission=Permission.STAFF
        )
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human="Fout bij beoordelen van resultaat."
            ),
            readable_expected=readable_expected,
            readable_actual="",
            messages=[staff_message, DEFAULT_STUDENT]
        )

    readable_actual = repr(actual_value)  # TODO: fix this.

    result = evaluate_programmed(
        config.bundle,
        evaluator=channel.evaluator,
        expected=expected_value,
        actual=actual_value,
        timeout=timeout
    )

    if result.timeout:
        return EvaluationResult(
            result=StatusMessage(enum=Status.TIME_LIMIT_EXCEEDED),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
            messages=[result.stdout, result.stderr]
        )

    if not result.stdout:
        stdout = ExtendedMessage(description=result.stdout, format="text")
        stderr = ExtendedMessage(description=result.stderr, format="text")
        return EvaluationResult(
            result=StatusMessage(enum=Status.INTERNAL_ERROR),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
            messages=[stdout, stderr, DEFAULT_STUDENT]
        )

    try:
        evaluation_result = _try_specific(result.stdout)
    except (TypeError, ValueError) as e:
        staff_message = ExtendedMessage(
            description=f"An error occurred parsing the result of the programmed "
                        f"evaluation. Received {result}, which caused {e}",
            format="text",
            permission=Permission.STAFF
        )
        return EvaluationResult(
            result=StatusMessage(enum=Status.INTERNAL_ERROR),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
            messages=[staff_message, DEFAULT_STUDENT]
        )

    if evaluation_result.readable_expected:
        readable_expected = evaluation_result.readable_expected
    if evaluation_result.readable_actual:
        readable_actual = evaluation_result.readable_actual

    return EvaluationResult(
        result=StatusMessage(
            enum=Status.CORRECT if evaluation_result.result else wrong
        ),
        readable_expected=readable_expected,
        readable_actual=readable_actual,
        messages=evaluation_result.messages
    )
