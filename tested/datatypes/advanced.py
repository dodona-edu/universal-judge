"""
Advanced data types.

Note that these are often complementary types: the default type suffice in many
cases.
"""
from enum import Enum
from typing import Union

from .basic import BasicNumericTypes, BasicSequenceTypes, BasicTypes


class _AdvancedDataType(str, Enum):
    """Represents the advanced data types."""

    def __new__(cls, *args, **kwargs):
        value = args[0]
        # noinspection PyArgumentList
        obj = str.__new__(cls, value)
        obj._value_ = args[0]
        return obj

    def __init__(self, _: str, base_type: BasicTypes):
        super().__init__()
        self._base_type_ = base_type

    @property
    def base_type(self) -> BasicTypes:
        return self._base_type_


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

    SINGLE_PRECISION = "single_precision", BasicNumericTypes.RATIONAL
    """IEEE 754 single precision real number."""
    DOUBLE_PRECISION = "double_precision", BasicNumericTypes.RATIONAL
    """IEEE 754 double precision real number."""
    DOUBLE_EXTENDED = "double_extended", BasicNumericTypes.RATIONAL
    """IEEE 754 double extended precision real number."""
    FIXED_PRECISION = "fixed_precision", BasicNumericTypes.RATIONAL
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


AdvancedTypes = Union[
    AdvancedNumericTypes, AdvancedSequenceTypes
]
