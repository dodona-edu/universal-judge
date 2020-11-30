"""
The data types.

The data type system is used in two ways:

1. As a describing system: the types are used to denote the datatype of actual data.
2. As a prescribing system: the types are used to denoted the channel type of some
   variable.

As mentioned in the manuscript, the context_testcase use is scenario 1.

Additionally, the types in this file are organised by their JSON encoding type.
They are also split in "basic types" and "advanced types".
"""

from .advanced import *
from .basic import *
from ..utils import get_args

NumericTypes = Union[BasicNumericTypes, AdvancedNumericTypes]
StringTypes = Union[BasicStringTypes]
BooleanTypes = Union[BasicBooleanTypes]
NothingTypes = Union[BasicNothingTypes]
SequenceTypes = Union[BasicSequenceTypes, AdvancedSequenceTypes]
ObjectTypes = Union[BasicObjectTypes]


SimpleTypes = Union[NumericTypes, StringTypes, BooleanTypes, NothingTypes]
ComplexTypes = Union[SequenceTypes, ObjectTypes]

AllTypes = Union[BasicTypes, AdvancedTypes]

# Test that our aliases are correct.
assert set(get_args(AllTypes)) == set(get_args(Union[SimpleTypes, ComplexTypes]))


def resolve_to_basic(type_: AllTypes) -> BasicTypes:
    """
    Resolve a type to its basic type. Basic types are returned unchanged.
    """
    if isinstance(type_, get_args(BasicTypes)):
        return type_

    assert isinstance(type_, get_args(AdvancedTypes))
    return type_.base_type


def string_to_type(type_string: str) -> AllTypes:
    enums = get_args(AllTypes)
    for enum in enums:
        if type_string in enum.__members__:
            return enum[type_string]
    raise ValueError(f"Unknown type string {type_string}")
