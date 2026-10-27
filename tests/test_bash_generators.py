import pytest

from tested.datatypes import BasicStringTypes
from tested.languages.bash.generators import convert_statement
from tested.serialisation import FunctionCall, FunctionType, StringType


@pytest.mark.parametrize(
    "arguments,expected",
    [
        ([], "greet"),
        ([StringType(type=BasicStringTypes.TEXT, data="World")], "greet World"),
        (
            [
                StringType(type=BasicStringTypes.TEXT, data="a b"),
                StringType(type=BasicStringTypes.TEXT, data="c"),
            ],
            "greet 'a b' c",
        ),
    ],
)
def test_function_call_has_no_trailing_space(arguments, expected):
    call = FunctionCall(type=FunctionType.FUNCTION, name="greet", arguments=arguments)
    assert convert_statement(call) == expected
