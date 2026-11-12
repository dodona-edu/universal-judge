import sys
from pathlib import Path
from types import SimpleNamespace

import pytest

from tested.configs import create_bundle
from tested.datatypes import (
    BasicBooleanTypes,
    BasicNumericTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.dodona import ExtendedMessage
from tested.features import (
    NOTHING,
    Construct,
    FeatureSet,
    SuiteFeatures,
    is_supported,
)
from tested.parsing import parse_json_suite
from tests.manual_utils import configuration


class StubLanguage:
    def __init__(
        self,
        required: FeatureSet,
        supported: set[Construct],
        restrictions: dict | None = None,
        expected_values: FeatureSet = NOTHING,
    ):
        self.config = SimpleNamespace(
            suite=SimpleNamespace(
                get_suite_features=lambda: SuiteFeatures(
                    generated_code=required, expected_values=expected_values
                ),
                tabs=[],
            )
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


def is_solvable(language) -> bool:
    return is_supported(language) is None


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
    assert "{'integer'}" in lines
    assert (
        "frozenset({<BasicBooleanTypes.BOOLEAN: 'boolean'>, "
        "<BasicNumericTypes.REAL: 'real'>, "
        "<BasicStringTypes.TEXT: 'text'>})"
    ) in lines


def test_constructs_of_expected_values_are_not_required():
    language = StubLanguage(
        NOTHING,
        set(),
        expected_values=FeatureSet(
            constructs={Construct.HETEROGENEOUS_COLLECTIONS},
            types=set(),
            nested_types=set(),
        ),
    )

    assert is_solvable(language)


def test_types_of_expected_values_are_required():
    language = StubLanguage(
        NOTHING,
        set(),
        expected_values=FeatureSet(
            constructs=set(),
            types={BasicNumericTypes.REAL},
            nested_types=set(),
        ),
    )

    lines = message_lines(language)

    assert "The test suite requires type real, which is not supported." in lines


SUITE_WITH_AN_OPTIONAL = """
{
  "tabs": [{
    "name": "optional",
    "contexts": [{
      "testcases": [{
        "input": {
          "type": "function",
          "name": "echo",
          "arguments": [ARGUMENT]
        },
        "output": {"result": {"value": VALUE}}
      }]
    }]
  }]
}
"""
INTEGER = '{"type": "integer", "data": 5}'
OPTIONAL = f'{{"type": "optional", "data": {INTEGER}}}'
SUITES = {
    "argument": SUITE_WITH_AN_OPTIONAL.replace("ARGUMENT", OPTIONAL).replace(
        "VALUE", INTEGER
    ),
    "expected-value": SUITE_WITH_AN_OPTIONAL.replace("ARGUMENT", INTEGER).replace(
        "VALUE", f'{{"type": "list", "data": [{OPTIONAL}]}}'
    ),
}


@pytest.mark.parametrize("suite_text", SUITES.values(), ids=SUITES.keys())
@pytest.mark.parametrize("language", ["haskell", "python"])
def test_a_suite_with_an_optional_value_is_not_supported(
    suite_text: str, language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    suite = parse_json_suite(suite_text)
    conf = configuration(pytestconfig, "", language, tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)

    lines = message_lines(bundle.language)

    missing = lines[lines.index("This means support is lacking for these:") + 1]
    assert "<Construct.OPTIONAL_VALUES: 'optional_values'>" in missing
