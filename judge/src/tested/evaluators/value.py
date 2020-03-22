"""
Value evaluator.
"""
import logging
from typing import Union, Tuple, Optional, get_args

from . import EvaluationResult, EvaluatorConfig
from ..configs import Bundle
from ..datatypes import AdvancedTypes, BasicTypes
from ..dodona import ExtendedMessage, Permission, StatusMessage, Status
from ..languages.generator import convert_expression
from ..serialisation import Value, parse_value, to_python_comparable, as_basic_type
from ..testplan import ValueOutputChannel, OutputChannel
from ..utils import Either

logger = logging.getLogger(__name__)


def try_as_value(value: str) -> Either[Value]:
    try:
        actual = parse_value(value)
        return Either(actual)
    except (ValueError, TypeError) as e:
        return Either(e)


def try_as_readable_value(bundle: Bundle, value: str) -> Optional[str]:
    try:
        actual = parse_value(value)
    except (ValueError, TypeError) as e:
        return None
    else:
        return convert_expression(bundle, actual)


def get_values(bundle: Bundle, output_channel: ValueOutputChannel, actual) \
        -> Union[EvaluationResult,
                 Tuple[Value, str, Optional[Value], str]]:
    expected = output_channel.value
    readable_expected = convert_expression(bundle, expected)

    # Special support for empty strings.
    if not actual.strip():
        return expected, readable_expected, None, ""

    # A crash here indicates a problem with one of the language implementations,
    # or a student is trying to cheat.
    try:
        actual = try_as_value(actual).get()
    except (TypeError, ValueError) as e:
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

    readable_actual = convert_expression(bundle, actual)
    return expected, readable_expected, actual, readable_actual


def _check_type(
        bundle: Bundle, expected: Value, actual: Value
) -> Tuple[bool, Value, Value]:
    """
    Check if the type of the two values match. The following procedure is used:

    1. If the expected value's type is a basic type, the actual value is reduced to
       it's basic type, after which both types are checked for equality.
    2. If the expected value's type is an advanced type, there are two
       possibilities:
       a. If the language of the submission supports the advanced type, both types
          are checked for equality. With "supports" we mean explicit support, not
          the fallback to the basic type.
       b. If the language of the submission does not support the advanced type, all
          types are reduced to their basic type, after which equality is checked.
          With "not support" we mean the language has the fallback.
       c. An error occurs if the language explicitly does not support the advanced
          type.

    :param bundle: The configuration bundle.
    :param expected: The expected type from the testplan.
    :param actual: The actual type produced by the plan.

    :return: A tuple with the result, expected value and actual value with the type
             that was used to do the check.
    """
    supported_types = bundle.language_config.type_support_map()

    # Case 2.c.
    if expected.type in supported_types and supported_types[expected.type] is None:
        raise ValueError(f"The language does not support {expected.type}")

    # Case 1.
    if isinstance(expected.type, get_args(BasicTypes)):
        logger.debug(f"The expected type {expected.type} is advanced.")
        basic_actual = as_basic_type(actual)
        return expected.type == basic_actual.type, expected, basic_actual

    assert isinstance(expected.type, get_args(AdvancedTypes))

    # Case 2.b.
    if expected.type not in supported_types:
        basic_expected = as_basic_type(expected)
        basic_actual = as_basic_type(actual)
        return (basic_expected.type == basic_actual.type,
                basic_expected, basic_actual)

    # Case 2.a.
    assert supported_types[expected.type] is not None

    return expected.type == actual.type, expected, actual


def evaluate(config: EvaluatorConfig,
             channel: OutputChannel, actual: str) -> EvaluationResult:
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
    result = get_values(config.bundle, channel, actual)
    if isinstance(result, EvaluationResult):
        return result
    else:
        expected, readable_expected, actual, readable_actual = result

    # If the channel value is not None, but actual is, error.
    if actual is None:
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human="Verwachtte een returnwaarde, maar er was er geen."
            ),
            readable_expected=readable_expected,
            readable_actual=readable_actual
        )

    type_check, expected, actual = _check_type(config.bundle, expected, actual)
    messages = []
    type_message = None
    if not type_check:
        type_message = "Returnwaarde heeft verkeerd type."
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
