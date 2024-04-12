"""
Advanced data types.

Note that these are often complementary types: the default type suffice in many
cases.
"""

from enum import StrEnum
from typing import Union

from tested.datatypes.basic import (
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    BasicTypes,
)


class _AdvancedDataType(StrEnum):
    """Represents the advanced data types."""

    base_type: BasicTypes

    def __new__(cls, value: str, base_type: BasicTypes):
        member = str.__new__(cls, value)
        member._value_ = value
        member.base_type = base_type
        return member


class AdvancedNumericTypes(_AdvancedDataType):
    """
    The advanced numeric types. Programming configs should be implemented using
    the C/C++ rules: the size of the types is a minimum. For example, Python's ints
    are arbitrary precision, which means Python supports all integer types.
    On the other hand, C only supports up to 64 bits.
    """

    INT_8 = "int8", BasicNumericTypes.INTEGER
    """Signed integer, 8 bits minimum."""
    U_INT_8 = "uint8", BasicNumericTypes.INTEGER
    """Unsigned integer, 8 bits minimum."""
    INT_16 = "int16", BasicNumericTypes.INTEGER
    """Unsigned integer, 16 bits minimum."""
    U_INT_16 = "uint16", BasicNumericTypes.INTEGER
    """Unsigned integer, 16 bits minimum."""
    INT_32 = "int32", BasicNumericTypes.INTEGER
    """Signed integer, 32 bits minimum."""
    U_INT_32 = "uint32", BasicNumericTypes.INTEGER
    """Unsigned integer, 32 bits minimum."""
    INT_64 = "int64", BasicNumericTypes.INTEGER
    """Signed integer, 64 bits minimum."""
    U_INT_64 = "uint64", BasicNumericTypes.INTEGER
    """Unsigned integer, 64 bits minimum."""
    BIG_INT = "bigint", BasicNumericTypes.INTEGER
    """Integer, arbitrary precision."""

    SINGLE_PRECISION = "single_precision", BasicNumericTypes.REAL
    """IEEE 754 single precision real number."""
    DOUBLE_PRECISION = "double_precision", BasicNumericTypes.REAL
    """IEEE 754 double precision real number."""
    DOUBLE_EXTENDED = "double_extended", BasicNumericTypes.REAL
    """IEEE 754 double extended precision real number."""
    FIXED_PRECISION = "fixed_precision", BasicNumericTypes.REAL
    """Fixed precision real number."""


class AdvancedSequenceTypes(_AdvancedDataType):
    """
    Advanced sequence types. The names of these types are kept as generic as
    possible, to accommodate as many types as possible.
    """

    ARRAY = "array", BasicSequenceTypes.SEQUENCE
    """
    An array is often a continuous piece of memory for elements of a fixed size.
    The difference is best described using Java, with its array and List types.
    A lot of configs only have LIST or ARRAY but not both.
    """
    LIST = "list", BasicSequenceTypes.SEQUENCE
    """
    A list structure is an ordered sequence of elements, where duplicates are
    allowed. See also the ARRAY type for a similar type.
    """
    TUPLE = "tuple", BasicSequenceTypes.SEQUENCE
    """A tuple is read-only, immutable list."""


class AdvancedStringTypes(_AdvancedDataType):
    CHAR = "char", BasicStringTypes.TEXT
    """
    A single character
    """
    STRING = "string", BasicStringTypes.TEXT
    """
    A string (sequence of characters).
    """


class AdvancedNothingTypes(_AdvancedDataType):
    UNDEFINED = "undefined", BasicNothingTypes.NOTHING
    """
    Distinguish between undefined values and null values.
    Like in JavaScript.
    """
    NULL = "null", BasicNothingTypes.NOTHING
    """
    Type to explicitly and exclusively denote the "null" type, but
    not the "undefined" type, as the basic "nothing" type does.
    """


class AdvancedObjectTypes(_AdvancedDataType):
    DICTIONARY = "dictionary", BasicObjectTypes.MAP
    """
    A proper map/dictionary/associative array data structure.
    """
    OBJECT = "object", BasicObjectTypes.MAP
    """
    An object like in JavaScript.
    """


AdvancedTypes = Union[
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
    AdvancedNothingTypes,
    AdvancedObjectTypes,
]
