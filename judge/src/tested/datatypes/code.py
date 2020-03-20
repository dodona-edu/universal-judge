"""
The datatypes for use in code.

These types can only be used when denoting the type of some literal in code, and
can never be used to describe encoded data.
"""
from enum import Enum


class CodeStringTypes(str, Enum):
    IDENTIFIER = "secret"
    """
    Reference to an element that was named earlier (variable or functions).
    """
