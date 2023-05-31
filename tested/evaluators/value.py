"""
Value evaluator.
"""
import logging
from typing import Optional, Tuple, Union

from tested.configs import Bundle
from tested.datatypes import (
    AdvancedTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    BasicTypes,
    SimpleTypes,
)
from tested.dodona import ExtendedMessage, Permission, Status, StatusMessage
from tested.evaluators.common import EvaluationResult, EvaluatorConfig
from tested.features import TypeSupport, fallback_type_support_map
from tested.internationalization import get_i18n_string
from tested.languages.generation import generate_statement
from tested.serialisation import (
    ObjectKeyValuePair,
    ObjectType,
    SequenceType,
    StringType,
    Value,
    as_basic_type,
    parse_value,
    to_python_comparable,
)
from tested.testsuite import OutputChannel, TextOutputChannel, ValueOutputChannel
from tested.utils import get_args, sorted_no_duplicates

logger = logging.getLogger(__name__)


def try_as_readable_value(
    bundle: Bundle, value: str
) -> Tuple[Optional[str], Optional[ExtendedMessage]]:
    try:
        actual = parse_value(value)
    except (ValueError, TypeError):
        return None, None
    else:
        return generate_statement(bundle, actual), None


def get_values(
    bundle: Bundle, output_channel: ValueOutputChannel, actual: str
) -> Union[EvaluationResult, Tuple[Value, str, Optional[Value], str]]:
    if isinstance(output_channel, TextOutputChannel):
        expected = output_channel.get_data_as_string(bundle.config.resources)
        expected_value = StringType(type=BasicStringTypes.TEXT, data=expected)
        actual_value = StringType(type=BasicStringTypes.TEXT, data=actual)
        return expected_value, expected, actual_value, actual

    assert isinstance(output_channel, ValueOutputChannel)

    expected = output_channel.value
    readable_expected = generate_statement(bundle, expected)

    # Special support for empty strings.
    if not actual.strip():
        return expected, readable_expected, None, ""

    # A crash here indicates a problem with one of the language implementations,
    # or a student is trying to cheat.
    try:
        actual = parse_value(actual)
    except (TypeError, ValueError) as e:
        raw_message = f"Received {actual}, which caused {e} for get_values."
        message = ExtendedMessage(
            description=raw_message, format="text", permission=Permission.STAFF
        )
        student = "An error occurred while collecting the return value. Contact staff for more information."
        return EvaluationResult(
            result=StatusMessage(enum=Status.INTERNAL_ERROR, human=student),
            readable_expected=readable_expected,
            readable_actual=str(actual),
            messages=[message],
        )

    # If the type is "unknown", we handle it here, instead of sending it through
    # the language configs.
    if actual.type == BasicStringTypes.UNKNOWN:
        if actual.diagnostic and actual.diagnostic not in actual.data:
            readable_actual = f"({actual.diagnostic}) {actual.data}"
        else:
            readable_actual = actual.data
        return EvaluationResult(
            result=StatusMessage(enum=Status.WRONG),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
        )

    readable_actual = generate_statement(bundle, actual)
    return expected, readable_expected, actual, readable_actual


def _check_type(bundle: Bundle, expected: Value, actual: Value) -> Tuple[bool, Value]:
    valid, value = check_data_type(bundle, expected, actual)
    if not valid:
        return False, value
    elif isinstance(value.type, get_args(SimpleTypes)):
        return True, value
    elif isinstance(value, SequenceType):
        if as_basic_type(value).type == BasicSequenceTypes.SET:
            actual_object = sorted_no_duplicates(
                value.data, recursive_key=lambda x: x.data
            )
            expected_object = sorted_no_duplicates(
                expected.data, recursive_key=lambda x: x.data
            )
        else:
            actual_object, expected_object = value.data, expected.data
        data = []
        for actual_element, expected_element in zip(actual_object, expected_object):
            element_valid, element_value = _check_type(
                bundle, expected_element, actual_element
            )
            valid = valid and element_valid
            data.append(element_value)
        value.data = data
        return valid, value
    else:
        assert isinstance(value, ObjectType)
        data = []
        actual_object = sorted_no_duplicates(
            value.data, key=lambda x: x.key, recursive_key=lambda x: x.data
        )
        expected_object = sorted_no_duplicates(
            expected.data, key=lambda x: x.key, recursive_key=lambda x: x.data
        )

        for actual_element, expected_element in zip(actual_object, expected_object):
            actual_key, actual_value = actual_element.key, actual_element.value
            expected_key, expected_value = actual_element.key, actual_element.value
            key_valid, key_value = _check_type(bundle, expected_key, actual_key)
            value_valid, value_value = _check_type(bundle, expected_value, actual_value)
            valid = valid and key_valid and value_valid
            data.append(ObjectKeyValuePair(key=key_value, value=value_value))
        value.data = data
        return valid, value


def check_data_type(
    bundle: Bundle, expected: Value, actual: Value
) -> Tuple[bool, Value]:
    """
    Check if the type of the two values match. The following procedure is used:

    1. If the expected value's type is a basic type, the actual value is reduced to
       it's basic type, after which both types are checked for equality.
    2. If the expected value's type is an advanced type, there are two
       possibilities:
       a. If the language of the submission supports the advanced type, both types
          are checked for equality.
       b. If the language of the submission only supports a reduction of the type,
          all types are reduced to their basic type, after which equality is
          checked.
    3. An error occurs if the language explicitly does not support the advanced
       type.

    :param bundle: The configuration bundle.
    :param expected: The expected type from the test suite.
    :param actual: The actual type produced by the suite.

    :return: A tuple with the result and expected value, the type that was used to
             do the check.
    """
    supported_types = fallback_type_support_map(bundle.lang_config)

    # Case 3.
    if supported_types[expected.type] == TypeSupport.UNSUPPORTED:
        raise ValueError(f"The language does not support {expected.type}")

    # Case 1.
    if isinstance(expected.type, get_args(BasicTypes)):
        basic_actual = as_basic_type(actual)
        return expected.type == basic_actual.type, expected

    assert isinstance(expected.type, get_args(AdvancedTypes))

    # Case 2.b.
    if supported_types[expected.type] == TypeSupport.REDUCED:
        basic_expected = as_basic_type(expected)
        basic_actual = as_basic_type(actual)
        return basic_expected.type == basic_actual.type, basic_expected

    # Case 2.a.
    assert supported_types[expected.type] == TypeSupport.SUPPORTED

    return expected.type == actual.type, expected


def evaluate(
    config: EvaluatorConfig, channel: OutputChannel, actual: str
) -> EvaluationResult:
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
    except (TypeError, ValueError):
        pass
    else:
        return evaluation_result

    # Try parsing the value as an actual Value.
    result = get_values(config.bundle, channel, actual)
    if isinstance(result, EvaluationResult):
        return result
    else:
        expected, readable_expected, actual, readable_actual = result

    # Special pretty printing for multiline strings.
    is_multiline_string = (
        config.options.get("stringsAsText", True)
        and expected.type == BasicStringTypes.TEXT
        and "\n" in expected.data
    )
    if is_multiline_string:
        readable_expected = get_as_string(expected, readable_expected)

    # If the channel value is not None, but actual is, error.
    if actual is None:
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.WRONG, human=get_i18n_string("evaluators.value.missing")
            ),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
        )

    type_check, expected = _check_type(config.bundle, expected, actual)
    messages = []
    type_status = None

    py_expected = to_python_comparable(expected)
    py_actual = to_python_comparable(actual)

    content_check = py_expected == py_actual

    # Only add the message about the type if the content is the same.
    if content_check and not type_check:
        type_status = get_i18n_string("evaluators.value.datatype.wrong")
        messages.append(
            get_i18n_string(
                "evaluators.value.datatype.message",
                expected=expected.type,
                actual=actual.type,
            )
        )

    correct = type_check and content_check

    if is_multiline_string:
        readable_actual = get_as_string(actual, readable_actual)

    return EvaluationResult(
        result=StatusMessage(
            human=type_status, enum=Status.CORRECT if correct else Status.WRONG
        ),
        readable_expected=readable_expected,
        readable_actual=readable_actual,
        messages=messages,
        is_multiline_string=is_multiline_string,
    )


def get_as_string(value: Optional[Value], readable: str):
    # Return readable if value is none
    if value is None:
        return readable
    # Replace tab by 4 spaces
    return (
        value.data.replace("\t", "    ")
        if value.type == BasicStringTypes.TEXT
        else readable
    )
