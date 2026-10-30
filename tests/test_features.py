from types import SimpleNamespace

from tested.datatypes import (
    BasicBooleanTypes,
    BasicNumericTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.dodona import ExtendedMessage
from tested.features import Construct, FeatureSet, is_supported


class StubLanguage:
    def __init__(
        self,
        required: FeatureSet,
        supported: set[Construct],
        restrictions: dict | None = None,
    ):
        self.config = SimpleNamespace(
            suite=SimpleNamespace(get_used_features=lambda: required, tabs=[])
        )
        self._supported = supported
        self._restrictions = restrictions or {}

    def supported_constructs(self):
        return self._supported

    def collection_restrictions(self):
        return self._restrictions

    def datatype_support(self):
        return {}


def message_lines(language) -> list[str]:
    messages = is_supported(language)
    assert messages is not None
    message = messages[0]
    assert isinstance(message, ExtendedMessage)
    return [line.strip() for line in message.description.splitlines()]


def test_unsupported_constructs_are_listed_sorted():
    required = {
        Construct.OBJECTS,
        Construct.EXCEPTIONS,
        Construct.FUNCTION_CALLS,
        Construct.ASSIGNMENTS,
        Construct.NAMED_ARGUMENTS,
        Construct.GLOBAL_VARIABLES,
    }
    supported = {Construct.FUNCTION_CALLS, Construct.OBJECTS}
    language = StubLanguage(
        FeatureSet(constructs=required, types=set(), nested_types=set()), supported
    )

    lines = message_lines(language)

    assert (
        "{<Construct.ASSIGNMENTS: 'assignments'>, "
        "<Construct.EXCEPTIONS: 'exceptions'>, "
        "<Construct.FUNCTION_CALLS: 'function_calls'>, "
        "<Construct.GLOBAL_VARIABLES: 'global_variables'>, "
        "<Construct.NAMED_ARGUMENTS: 'named_arguments'>, "
        "<Construct.OBJECTS: 'objects'>}"
    ) in lines
    assert (
        "{<Construct.FUNCTION_CALLS: 'function_calls'>, <Construct.OBJECTS: 'objects'>}"
    ) in lines
    assert (
        "{<Construct.ASSIGNMENTS: 'assignments'>, "
        "<Construct.EXCEPTIONS: 'exceptions'>, "
        "<Construct.GLOBAL_VARIABLES: 'global_variables'>, "
        "<Construct.NAMED_ARGUMENTS: 'named_arguments'>}"
    ) in lines


def test_unsupported_collection_types_are_listed_sorted():
    value_types = frozenset(
        {
            BasicNumericTypes.REAL,
            BasicStringTypes.TEXT,
            BasicBooleanTypes.BOOLEAN,
            BasicNumericTypes.INTEGER,
        }
    )
    restrictions = {BasicNumericTypes.INTEGER}
    language = StubLanguage(
        FeatureSet(
            constructs=set(),
            types=set(),
            nested_types={(BasicSequenceTypes.SET, value_types)},
        ),
        set(),
        {BasicSequenceTypes.SET: restrictions},
    )

    lines = message_lines(language)

    assert (
        "frozenset({<BasicBooleanTypes.BOOLEAN: 'boolean'>, "
        "<BasicNumericTypes.INTEGER: 'integer'>, "
        "<BasicNumericTypes.REAL: 'real'>, "
        "<BasicStringTypes.TEXT: 'text'>})"
    ) in lines
    assert "{<BasicNumericTypes.INTEGER: 'integer'>}" in lines
    assert (
        "frozenset({<BasicBooleanTypes.BOOLEAN: 'boolean'>, "
        "<BasicNumericTypes.REAL: 'real'>, "
        "<BasicStringTypes.TEXT: 'text'>})"
    ) in lines
