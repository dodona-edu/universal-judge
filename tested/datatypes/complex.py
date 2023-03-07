from enum import Enum


class ComplexExpressionTypes(str, Enum):
    """
    These datatypes are only used in the language configs, not in the test_suite
    """

    FUNCTION_CALLS = "function_calls"
    IDENTIFIERS = "identifiers"
