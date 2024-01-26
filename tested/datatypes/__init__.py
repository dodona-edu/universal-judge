"""
The data types.

The data type system is used in two ways:

1. As a describing system: the types are used to denote the datatype of actual data.
2. As a prescribing system: the types are used to denote the channel type of some
   variable.

As mentioned in the manuscript, the context_testcase use is scenario 1.

Additionally, the types in this file are organized by their JSON encoding type.
They are also split in "basic types" and "advanced types".
"""
from tested.datatypes.advanced import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
    AdvancedTypes,
)
from tested.datatypes.basic import (
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    BasicTypes,
)
from tested.datatypes.complex import ComplexExpressionTypes
from tested.utils import get_args

NumericTypes = BasicNumericTypes | AdvancedNumericTypes
StringTypes = BasicStringTypes | AdvancedStringTypes
BooleanTypes = BasicBooleanTypes
NothingTypes = BasicNothingTypes | AdvancedNothingTypes
SequenceTypes = BasicSequenceTypes | AdvancedSequenceTypes
ObjectTypes = BasicObjectTypes

SimpleTypes = NumericTypes | StringTypes | BooleanTypes | NothingTypes
ComplexTypes = SequenceTypes | ObjectTypes

AllTypes = BasicTypes | AdvancedTypes

ExpressionTypes = AllTypes | ComplexExpressionTypes
NestedTypes = set[tuple[ExpressionTypes, frozenset[ExpressionTypes]]]

# Test that our aliases are correct.
assert set(get_args(AllTypes)) == set(get_args(SimpleTypes | ComplexTypes))


def resolve_to_basic(type_: AllTypes) -> BasicTypes:
    """
    Resolve a type to its basic type. Basic types are returned unchanged.
    """
    if isinstance(type_, BasicTypes):
        return type_

    assert isinstance(type_, AdvancedTypes)
    return type_.base_type


def string_to_type(type_string: str) -> AllTypes:
    enums = get_args(AllTypes)
    for enum in enums:
        try:
            return enum(type_string)
        except ValueError:
            pass
    raise ValueError(f"Unknown type string {type_string}")
