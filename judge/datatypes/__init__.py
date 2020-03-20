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
from typing import Union, get_args

from datatypes.advanced import AdvancedNumericTypes, AdvancedSequenceTypes, \
    AdvancedObjectTypes, AdvancedTypes
from datatypes.basic import BasicNumericTypes, BasicStringTypes, \
    BasicBooleanTypes, BasicSequenceTypes, BasicObjectTypes, BasicNothingTypes, \
    BasicTypes
from datatypes.code import CodeStringTypes

NumericTypes = Union[BasicNumericTypes, AdvancedNumericTypes]
StringTypes = BasicStringTypes
ExtendedStringTypes = Union[StringTypes, CodeStringTypes]
BooleanTypes = BasicBooleanTypes
SequenceTypes = Union[BasicSequenceTypes, AdvancedSequenceTypes]
ObjectTypes = Union[BasicObjectTypes, AdvancedObjectTypes]
NothingTypes = BasicNothingTypes

SimpleTypes = Union[NumericTypes, StringTypes, BooleanTypes, NothingTypes]
ComplexTypes = Union[SequenceTypes, ObjectTypes]

AllTypes = Union[BasicTypes, AdvancedTypes]

ExtendedTypes = Union[
    NumericTypes, ExtendedStringTypes, BooleanTypes, NothingTypes, ComplexTypes]


def resolve_to_basic(type_: AllTypes) -> BasicTypes:
    """
    Resolve a type to its basic type. Basic types are returned unchanged.
    """
    if isinstance(type_, get_args(BasicTypes)):
        return type_

    assert isinstance(type_, get_args(AdvancedTypes))
    return type_.base_type
