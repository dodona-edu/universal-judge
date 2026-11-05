import pytest

from tested.datatypes import (
    AdvancedNumericTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.dsl.ast_translator import parse_string
from tested.languages.csharp.generators import convert_statement, convert_value
from tested.serialisation import (
    NumberType,
    ObjectKeyValuePair,
    ObjectType,
    SequenceType,
    StringType,
)


@pytest.mark.parametrize(
    "source,full,expected",
    [
        (
            "f({(1, 2), (2, 3)})",
            False,
            "f(new HashSet<(Int32, Int32)>() {(1, 2), (2, 3)})",
        ),
        ("f([(1, 2)])", False, "f(new List<(Int32, Int32)>() {(1, 2)})"),
        (
            "f({'a': (1, 2)})",
            False,
            'f(new Dictionary<string, (Int32, Int32)>() {{"a", (1, 2)}})',
        ),
        ("x = (1, 2)", True, "(Int32, Int32) x = (1, 2)"),
        ("x = (1, 'a')", True, '(Int32, string) x = (1, "a")'),
        ("f({1, 2})", False, "f(new HashSet<Int32>() {1, 2})"),
        (
            "f([[(1, 2)]])",
            False,
            "f(new List<List<(Int32, Int32)>>() {new List<(Int32, Int32)>() {(1, 2)}})",
        ),
    ],
)
def test_convert_statement(source, full, expected):
    assert convert_statement(parse_string(source), full=full) == expected


@pytest.mark.parametrize(
    "value,expected",
    [
        (
            SequenceType(
                type=BasicSequenceTypes.SEQUENCE,
                data=[
                    NumberType(type=BasicNumericTypes.INTEGER, data=1),
                    StringType(type=BasicStringTypes.UNKNOWN, data="Foo{x=1}"),
                ],
            ),
            "new List<Object>() {1, Foo{x=1}}",
        ),
        (
            SequenceType(
                type=BasicSequenceTypes.SEQUENCE,
                data=[StringType(type=BasicStringTypes.UNKNOWN, data="Foo{x=1}")],
            ),
            "new List<Object>() {Foo{x=1}}",
        ),
        (
            ObjectType(
                type=BasicObjectTypes.MAP,
                data=[
                    ObjectKeyValuePair(
                        NumberType(type=BasicNumericTypes.INTEGER, data=1),
                        StringType(type=BasicStringTypes.UNKNOWN, data="Foo{x=1}"),
                    )
                ],
            ),
            "new Dictionary<Int32, Object>() {{1, Foo{x=1}}}",
        ),
        (NumberType(type=AdvancedNumericTypes.INT_32, data=5), "5"),
    ],
)
def test_convert_value(value, expected):
    assert convert_value(value) == expected
