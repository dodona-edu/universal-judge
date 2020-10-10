"""
Basic data types.
"""
from enum import Enum
from typing import Union


class BasicNumericTypes(str, Enum):
    INTEGER = "integer"
    RATIONAL = "rational"


class BasicStringTypes(str, Enum):
    CHAR = "character"
    TEXT = "text"
    ANY = "any"  # Cannot be used in testplan.


class BasicBooleanTypes(str, Enum):
    BOOLEAN = "boolean"


class BasicSequenceTypes(str, Enum):
    SEQUENCE = "sequence"
    SET = "set"


class BasicObjectTypes(str, Enum):
    MAP = "map"


class BasicNothingTypes(str, Enum):
    NOTHING = "nothing"


BasicTypes = Union[
    BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicObjectTypes,
    BasicNothingTypes, BasicSequenceTypes
]
