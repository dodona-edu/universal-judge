"""
Basic data types.
"""
from enum import StrEnum, auto, unique
from typing import Union


@unique
class BasicNumericTypes(StrEnum):
    INTEGER = auto()
    REAL = auto()


@unique
class BasicStringTypes(StrEnum):
    TEXT = auto()
    ANY = auto()  # Cannot be used in a test suite.
    UNKNOWN = auto()  # Cannot be used in a test suite.


@unique
class BasicBooleanTypes(StrEnum):
    BOOLEAN = auto()


@unique
class BasicSequenceTypes(StrEnum):
    SEQUENCE = auto()
    SET = auto()


@unique
class BasicObjectTypes(StrEnum):
    MAP = auto()


@unique
class BasicNothingTypes(StrEnum):
    NOTHING = auto()


BasicTypes = Union[
    BasicNumericTypes,
    BasicStringTypes,
    BasicBooleanTypes,
    BasicObjectTypes,
    BasicNothingTypes,
    BasicSequenceTypes,
]
