import pytest

from tested.datatypes import BasicNumericTypes, BasicSequenceTypes, BasicStringTypes
from tested.languages.haskell.generators import convert_statement
from tested.serialisation import (
    Expression,
    FunctionCall,
    FunctionType,
    NumberType,
    OptionalType,
    SequenceType,
    StringType,
    Value,
)


def _integer(data: int) -> NumberType:
    return NumberType(type=BasicNumericTypes.INTEGER, data=data)


def _optional(data: Value | OptionalType | None) -> OptionalType:
    return OptionalType(type="optional", data=data)


def _list(*data: Expression) -> SequenceType:
    return SequenceType(type=BasicSequenceTypes.SEQUENCE, data=list(data))


@pytest.mark.parametrize(
    "value,expected",
    [
        (_optional(_integer(7)), "Just 7"),
        (_optional(_integer(-3)), "Just (-3)"),
        (_optional(_optional(_integer(3))), "Just (Just 3)"),
        (_optional(_optional(None)), "Just Nothing"),
        (_optional(None), "Nothing"),
        (_optional(_list(_integer(1), _integer(2))), "Just [1, 2]"),
        (
            _optional(StringType(type=BasicStringTypes.TEXT, data="a b")),
            'Just "a b"',
        ),
        (_list(_optional(_integer(2)), _optional(None)), "[Just 2, Nothing]"),
        (
            _optional(
                StringType(type=BasicStringTypes.UNKNOWN, data="x", diagnostic="Foo")
            ),
            "Just ((Foo) x)",
        ),
    ],
)
def test_optional_values(value: Expression, expected: str):
    assert convert_statement(value, display=True) == expected


def test_present_optional_argument_is_parenthesised():
    call = FunctionCall(
        type=FunctionType.FUNCTION, name="f", arguments=[_optional(_integer(5))]
    )
    assert convert_statement(call) == "f (Just 5)"


def test_absent_optional_argument_is_bare():
    call = FunctionCall(
        type=FunctionType.FUNCTION, name="f", arguments=[_optional(None)]
    )
    assert convert_statement(call) == "f Nothing"
