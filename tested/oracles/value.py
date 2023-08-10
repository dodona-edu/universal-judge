"""
Value oracle.
"""
import logging
from typing import Optional, Tuple, Union, cast

from tested.configs import Bundle
from tested.datatypes import (
    AdvancedTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    BasicTypes,
    SimpleTypes,
)
from tested.dodona import ExtendedMessage, Message, Permission, Status, StatusMessage
from tested.features import TypeSupport, fallback_type_support_map
from tested.internationalization import get_i18n_string
from tested.languages.generation import generate_statement
from tested.oracles.common import OracleConfig, OracleResult
from tested.parsing import get_converter
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
from tested.testsuite import (
    OracleOutputChannel,
    OutputChannel,
    TextOutputChannel,
    ValueOutputChannel,
)
from tested.utils import sorted_no_duplicates

logger = logging.getLogger(__name__)


def try_as_readable_value(
    bundle: Bundle, value: str
) -> Tuple[Optional[str], Optional[Message]]:
    try:
        actual = parse_value(value)
    except (ValueError, TypeError):
        return None, None
    else:
        return generate_statement(bundle, actual), None


def get_values(
    bundle: Bundle, output_channel: OracleOutputChannel, actual_str: str
) -> Union[OracleResult, Tuple[Value, str, Optional[Value], str]]:
    if isinstance(output_channel, TextOutputChannel):
        expected = output_channel.get_data_as_string(bundle.config.resources)
        expected_value = StringType(type=BasicStringTypes.TEXT, data=expected)
        actual_value = StringType(type=BasicStringTypes.TEXT, data=actual_str)
        return expected_value, expected, actual_value, actual_str

    assert isinstance(output_channel, ValueOutputChannel)

    expected = output_channel.value
    assert isinstance(expected, Value)
    readable_expected = generate_statement(bundle, expected)

    # Special support for empty strings.
    if not actual_str.strip():
        return expected, readable_expected, None, ""

    # A crash here indicates a problem with one of the language implementations,
    # or a student is trying to cheat.
    try:
        actual = parse_value(actual_str)
    except (TypeError, ValueError) as e:
        raw_message = f"Received {actual_str}, which caused {e} for get_values."
        message = ExtendedMessage(
            description=raw_message, format="text", permission=Permission.STAFF
        )
        student = "An error occurred while collecting the return value. Contact staff for more information."
        return OracleResult(
            result=StatusMessage(enum=Status.INTERNAL_ERROR, human=student),
            readable_expected=readable_expected,
            readable_actual=actual_str,
            messages=[message],
        )

    readable_actual = generate_statement(bundle, actual)
    return expected, readable_expected, actual, readable_actual


def _prepare_value_for_type_check(value: Value) -> Value:
    """
    Prepare a value for type checking.

    This is mainly sorting the values if the value is a container. Note that we don't
    actually do type checking here.

    :param value: The value to prepare.
    :return: The prepared value.
    """
    if isinstance(value, SequenceType):
        basic_type = as_basic_type(value)
        if basic_type.type == BasicSequenceTypes.SET:
            value.data = sorted_no_duplicates(
                value.data, recursive_key=lambda x: x.data  # type: ignore
            )
    elif isinstance(value, ObjectType):
        assert isinstance(value, ObjectType)
        value.data = sorted_no_duplicates(
            value.data, key=lambda x: x.key, recursive_key=lambda x: x.data  # type: ignore
        )
    else:
        assert isinstance(value.type, SimpleTypes)
    return value


def _check_simple_type(
    bundle: Bundle, expected: Value, actual: Value
) -> Tuple[bool, Value]:
    """
    Check if the data type of two simple values match. The following procedure is used:

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
    :param expected: The expected value from the test suite.
    :param actual: The actual value produced by the suite.

    :return: A tuple with the result and expected value, which can have a modified type.
    """
    supported_types = fallback_type_support_map(bundle.lang_config)

    # Case 3.
    if supported_types[expected.type] == TypeSupport.UNSUPPORTED:
        raise ValueError(f"The language does not support {expected.type}")

    # Case 1.
    if isinstance(expected.type, BasicTypes):
        basic_actual = as_basic_type(actual)
        return expected.type == basic_actual.type, expected

    assert isinstance(expected.type, AdvancedTypes)

    # Case 2.b.
    if supported_types[expected.type] == TypeSupport.REDUCED:
        basic_expected = as_basic_type(expected)
        basic_actual = as_basic_type(actual)
        return basic_expected.type == basic_actual.type, basic_expected

    # Case 2.a.
    assert supported_types[expected.type] == TypeSupport.SUPPORTED

    return expected.type == actual.type, expected


def _check_data_type(
    bundle: Bundle, expected: Value, actual: Optional[Value]
) -> Tuple[bool, Value]:
    """
    Check if two values have the same (recursive) type.

    Recursive in this context means that all container types need to have elements
    with matching types as well. For example, a list of integers will only match
    another list of integers.

    :param bundle: The configuration bundle.
    :param expected: The expected value from the test suite.
    :param actual: The actual value produced by the suite.

    :return: A tuple with the result and expected value, which can have a modified type.
    """
    prepared_expected = _prepare_value_for_type_check(expected)

    if actual is None:
        return False, prepared_expected

    prepared_actual = _prepare_value_for_type_check(actual)

    valid, prepared_expected = _check_simple_type(
        bundle, prepared_expected, prepared_actual
    )

    if isinstance(prepared_expected, SequenceType):
        expected_elements = prepared_expected.data
        actual_elements = (
            prepared_actual.data if isinstance(prepared_actual.data, list) else []
        )
        prepared_elements = []
        for expected_element, actual_element in zip(expected_elements, actual_elements):
            assert expected_element is None or isinstance(expected_element, Value)
            assert actual_element is None or isinstance(actual_element, Value)
            element_valid, prepared_element = _check_data_type(
                bundle, expected_element, actual_element
            )
            prepared_elements.append(prepared_element)
            valid = valid and element_valid
        prepared_expected.data = prepared_elements
    elif isinstance(prepared_expected, ObjectType):
        expected_elements = prepared_expected.data
        actual_elements = (
            prepared_actual.data if isinstance(prepared_actual.data, list) else []
        )
        prepared_elements = []
        for expected_element, actual_element in zip(expected_elements, actual_elements):
            assert isinstance(actual_element, ObjectKeyValuePair)
            actual_key, actual_value = actual_element.key, actual_element.value
            assert isinstance(actual_key, Value) and isinstance(actual_value, Value)
            expected_key, expected_value = expected_element.key, expected_element.value
            assert isinstance(expected_key, Value) and isinstance(expected_value, Value)
            key_valid, prepared_key = _check_data_type(bundle, expected_key, actual_key)
            value_valid, prepared_value = _check_data_type(
                bundle, expected_value, actual_value
            )
            valid = valid and key_valid and value_valid
            prepared_elements.append(
                ObjectKeyValuePair(key=prepared_key, value=prepared_value)
            )
        prepared_expected.data = prepared_elements
    else:
        assert isinstance(prepared_expected.type, SimpleTypes)
    return valid, prepared_expected


def evaluate(
    config: OracleConfig, channel: OutputChannel, actual_str: str
) -> OracleResult:
    """
    Evaluate two values. The values must match exact. Currently, this oracle
    has no options, but it might receive them in the future (e.g. options on how
    to deal with strings or floats).
    """
    assert isinstance(channel, ValueOutputChannel)

    # Try parsing the value as an OracleResult.
    # This is the result of a custom oracle.
    # noinspection PyBroadException
    try:
        evaluation_result = get_converter().loads(actual_str, OracleResult)
    except Exception:
        pass
    else:
        return evaluation_result

    # Try parsing the value as an actual Value.
    result = get_values(config.bundle, channel, actual_str)
    if isinstance(result, OracleResult):
        return result
    else:
        expected, readable_expected, actual, readable_actual = result

    # Special pretty printing for multiline strings.
    is_multiline_string = (
        config.options.get("stringsAsText", True)
        and expected.type == BasicStringTypes.TEXT
        and "\n" in cast(str, expected.data)
    )
    if is_multiline_string:
        readable_expected = get_as_string(expected, readable_expected)

    # If the channel value is not None, but actual is, error.
    if actual is None:
        return OracleResult(
            result=StatusMessage(
                enum=Status.WRONG, human=get_i18n_string("functions.value.missing")
            ),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
        )

    type_check, expected = _check_data_type(config.bundle, expected, actual)
    messages = []
    type_status = None

    py_expected = to_python_comparable(expected)
    py_actual = to_python_comparable(actual)

    content_check = py_expected == py_actual
    correct = type_check and content_check

    if is_multiline_string:
        readable_actual = get_as_string(actual, readable_actual)

    # If the displayed values are the same, add a message about the type.
    if not type_check and readable_expected == readable_actual:
        type_status = get_i18n_string("evaluators.value.datatype.wrong")
        messages.append(
            get_i18n_string(
                "evaluators.value.datatype.message",
                expected=expected.type,
                actual=actual.type,
            )
        )

    return OracleResult(
        result=StatusMessage(
            human=type_status, enum=Status.CORRECT if correct else Status.WRONG
        ),
        readable_expected=readable_expected,
        readable_actual=readable_actual,
        messages=messages,
        is_multiline_string=is_multiline_string,
    )


def get_as_string(value: Optional[Value], readable: str) -> str:
    # Return readable if value is none
    if value is None:
        return readable
    if value.type == BasicStringTypes.TEXT:
        assert isinstance(value, StringType)
        # Replace tab by 4 spaces
        return value.data.replace("\t", "    ")
    else:
        return readable
