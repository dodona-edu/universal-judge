"""Bash testcase descriptions must not end in a space (#744).

Every argument in the bash function-call description path was emitted with a
trailing space, which stayed at the end of the description: `greet World `
instead of `greet World`.
"""

from tested.datatypes import BasicStringTypes
from tested.languages.bash.generators import (
    convert_function_call,
    unique_index_function,
)
from tested.serialisation import FunctionCall, FunctionType, StringType


def _call(*arguments):
    return FunctionCall(
        type=FunctionType.FUNCTION, name="greet", arguments=list(arguments)
    )


def _text(value: str) -> StringType:
    return StringType(type=BasicStringTypes.TEXT, data=value)


def test_description_with_one_argument_has_no_trailing_space():
    result = convert_function_call(_call(_text("World")), unique_index_function(), [])
    assert result == "greet World"


def test_description_with_two_arguments_has_no_trailing_space():
    result = convert_function_call(
        _call(_text("World"), _text("Dodona")), unique_index_function(), []
    )
    assert result == "greet World Dodona"


def test_description_without_arguments_has_no_trailing_space():
    result = convert_function_call(_call(), unique_index_function(), [])
    assert result == "greet"


def test_property_call_is_unchanged():
    call = FunctionCall(type=FunctionType.PROPERTY, name="length", arguments=[])
    result = convert_function_call(call, unique_index_function(), [])
    assert result == '"$length"'
