from enum import StrEnum, auto, unique


@unique
class ComplexExpressionTypes(StrEnum):
    """
    These datatypes are only used in the language configs, not in the test suite
    """

    FUNCTION_CALLS = auto()
    IDENTIFIERS = auto()
