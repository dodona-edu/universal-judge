"""
Value evaluator.
"""
import logging
from typing import Union, Tuple

from tested.dodona import ExtendedMessage, Permission, StatusMessage, Status
from evaluators import EvaluationResult
from testplan.channels import ValueOutputChannel, OutputChannel
from tested.serialisation import Value, get_readable_representation, \
    parse_value, SerialisationError, to_python_comparable
from tested.utils import Either

logger = logging.getLogger(__name__)


def try_as_value(value: str) -> Either[Value]:
    try:
        actual = parse_value(value)
        return Either(actual)
    except (ValueError, TypeError) as e:
        return Either(e)


def get_values(output_channel: ValueOutputChannel, actual) \
        -> Union[EvaluationResult,
                 Tuple[Value, str, Value, str]]:
    expected = output_channel.value
    readable_expected = get_readable_representation(expected)

    # A crash here indicates a problem with one of the language implementations,
    # or a student is trying to cheat.
    try:
        actual = parse_value(actual) if actual else None
        readable_actual = get_readable_representation(actual) if actual else ""
    except SerialisationError as e:
        raw_message = f"Received {actual}, which caused {e} for get_values."
        message = ExtendedMessage(
            description=raw_message,
            format="text",
            permission=Permission.STAFF
        )
        student = "Your return value was wrong; additionally Dodona didn't " \
                  "recognize it. Contact staff for more information."
        return EvaluationResult(
            result=StatusMessage(enum=Status.WRONG, human=student),
            readable_expected=readable_expected,
            readable_actual=str(actual),
            messages=[message]
        )

    return expected, readable_expected, actual, readable_actual


def evaluate(_, channel: OutputChannel, actual: str) -> EvaluationResult:
    """
    Evaluate two values. The values must match exact. Currently, this evaluator
    has no options, but it might receive them in the future (e.g. options on how
    to deal with strings or floats).
    """
    assert isinstance(channel, ValueOutputChannel)

    # Try parsing the value as an EvaluationResult.
    # This is the result of a custom evaluator.
    try:
        evaluation_result = EvaluationResult.__pydantic_model__.parse_raw(actual)
    except (TypeError, ValueError) as e:
        logger.warning("An error occurred while parsing as evaluation result.")
        logger.warning(e)
    else:
        return evaluation_result

    # Try parsing the value as an actual Value.

    result = get_values(channel, actual)
    if isinstance(result, EvaluationResult):
        return result
    else:
        expected, readable_expected, actual, readable_actual = result

    # If the channel value is None, and the actual value not, error.
    if expected is None and actual is not None:
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human="Verwachtte geen returnwaarde, maar er was er toch een."
            ),
            readable_expected=readable_expected,
            readable_actual=readable_actual
        )

    # If the channel value is not None, but actual is, error.
    if expected is not None and actual is None:
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human="Verwachtte een returnwaarde, maar er was er geen."
            ),
            readable_expected=readable_expected,
            readable_actual=readable_actual
        )

    messages = []
    type_check = True
    type_message = None
    # noinspection PyUnresolvedReferences
    if (expected is not None and actual is not None) and (
            expected.type != actual.type):
        type_message = "Returnwaarde heeft verkeerd type."
        type_check = False
        messages.append(
            f"Verwachtte waarde van type {expected.type}, "
            f"maar was type {actual.type}."
        )

    expected = to_python_comparable(expected)
    actual = to_python_comparable(actual)

    correct = type_check and expected == actual

    return EvaluationResult(
        result=StatusMessage(
            human=type_message,
            enum=Status.CORRECT if correct else Status.WRONG
        ),
        readable_expected=readable_expected,
        readable_actual=readable_actual,
        messages=messages
    )
