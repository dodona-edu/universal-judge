import sys
from pathlib import Path

import pytest
from attrs import define

from tested.configs import DodonaConfig, GlobalConfig, create_bundle
from tested.datatypes import (
    AdvancedNumericTypes,
    AdvancedObjectTypes,
    AllTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
)
from tested.dodona import ExtendedMessage, Permission
from tested.dsl import parse_dsl
from tested.features import (
    _CONSTRUCT_DESCRIPTIONS,
    NOTHING,
    Construct,
    FeatureSet,
    NestedTypes,
    SuiteFeatures,
    is_supported,
)
from tested.languages.c.config import C
from tested.languages.haskell.config import Haskell
from tested.languages.javascript.config import JavaScript
from tested.languages.language import Language
from tested.languages.python.config import Python
from tested.parsing import parse_json_suite
from tested.testsuite import (
    Context,
    LanguageLiterals,
    Suite,
    SupportedLanguage,
    Tab,
    Testcase,
)
from tests.manual_utils import configuration


@define
class FeatureSuite(Suite):
    features: FeatureSet = NOTHING
    expected_values: FeatureSet = NOTHING

    def get_suite_features(self) -> SuiteFeatures:
        return SuiteFeatures(
            generated_code=self.features, expected_values=self.expected_values
        )


def make_language(
    language_class: type[Language],
    programming_language: SupportedLanguage,
    constructs: set[Construct] | None = None,
    types: set[AllTypes] | None = None,
    nested_types: NestedTypes | None = None,
    tabs: list[Tab] | None = None,
    expected_values: FeatureSet = NOTHING,
) -> Language:
    features = FeatureSet(
        constructs=constructs or set(),
        types=types or set(),
        nested_types=nested_types or set(),
    )
    dodona = DodonaConfig(
        resources=Path("."),
        source=Path("submission"),
        time_limit=10,
        memory_limit=1024,
        natural_language="en",
        programming_language=programming_language,
        workdir=Path("."),
        judge=Path("."),
    )
    config = GlobalConfig(
        dodona=dodona,
        testcase_separator_secret="a",
        context_separator_secret="b",
        suite=FeatureSuite(
            tabs=tabs or [], features=features, expected_values=expected_values
        ),
    )
    return language_class(config)


def description(language: Language) -> str:
    messages = is_supported(language)
    assert messages is not None
    assert len(messages) == 1
    message = messages[0]
    assert isinstance(message, ExtendedMessage)
    assert message.format == "text"
    assert message.permission == Permission.EDUCATOR
    return message.description


def test_missing_construct_with_advice():
    language = make_language(
        Haskell,
        SupportedLanguage.HASKELL,
        constructs={Construct.FUNCTION_CALLS, Construct.HETEROGENEOUS_COLLECTIONS},
    )

    assert description(language) == (
        "This test suite is not solvable in Haskell, because Haskell does not "
        "support:\n"
        '- a collection with elements of different types, such as ["a", 1]\n'
        '  If the values form a fixed group, write them as a tuple: ("a", 1).'
    )


def test_missing_heterogeneous_collections_without_tuple_advice():
    language = make_language(
        C,
        SupportedLanguage.C,
        constructs={Construct.FUNCTION_CALLS, Construct.HETEROGENEOUS_COLLECTIONS},
    )

    assert description(language) == (
        "This test suite is not solvable in C, because C does not support:\n"
        '- a collection with elements of different types, such as ["a", 1]'
    )


def test_missing_constructs_follow_definition_order():
    language = make_language(
        C,
        SupportedLanguage.C,
        constructs={
            Construct.NAMED_ARGUMENTS,
            Construct.OBJECTS,
            Construct.EXCEPTIONS,
            Construct.FUNCTION_CALLS,
            Construct.DEFAULT_PARAMETERS,
        },
    )

    assert description(language) == (
        "This test suite is not solvable in C, because C does not support:\n"
        "- objects: constructors, methods or properties\n"
        "- expected exceptions\n"
        "- calling the same function with different numbers of arguments, "
        "such as f(1) and f(1, 2)\n"
        '- named arguments, such as f(name="a")\n'
        "  Pass the arguments by position instead."
    )


def test_every_construct_has_a_message():
    assert set(_CONSTRUCT_DESCRIPTIONS) == set(Construct)


def test_one_unsupported_basic_type():
    language = make_language(
        C, SupportedLanguage.C, types={BasicSequenceTypes.SEQUENCE}
    )

    assert description(language) == (
        "This test suite is not solvable in C, because C does not support the "
        "type sequence."
    )


def test_unsupported_types_mention_supported_basic_type():
    language = make_language(
        C,
        SupportedLanguage.C,
        types={
            BasicSequenceTypes.SEQUENCE,
            AdvancedNumericTypes.BIG_INT,
            BasicNumericTypes.INTEGER,
        },
    )

    assert description(language) == (
        "This test suite is not solvable in C, because C does not support the "
        "types bigint and sequence.\n"
        "C does support the less specific type integer instead of bigint."
    )


def test_language_specific_code_for_other_languages():
    testcase = Testcase(
        input=LanguageLiterals(
            literals={
                SupportedLanguage.PYTHON: "1",
                SupportedLanguage.JAVASCRIPT: "1",
            }
        )
    )
    tab = Tab(name="Tab", contexts=[Context(testcases=[testcase])])
    language = make_language(Haskell, SupportedLanguage.HASKELL, tabs=[tab])

    assert description(language) == (
        "This test suite is not solvable in Haskell, because its "
        "language-specific code exists only for javascript and python.\n"
        "Add a haskell version to make it solvable in Haskell."
    )


def test_set_elements_not_supported():
    language = make_language(
        Python,
        SupportedLanguage.PYTHON,
        nested_types={
            (BasicSequenceTypes.SET, frozenset({BasicSequenceTypes.SEQUENCE}))
        },
    )

    assert description(language) == (
        "This test suite is not solvable in Python, because Python does not "
        "support set elements of type sequence.\n"
        "Set elements in Python can be: bigint, boolean, fixed_precision, "
        "integer, real, text, tuple."
    )


def test_object_keys_not_supported():
    language = make_language(
        JavaScript,
        SupportedLanguage.JAVASCRIPT,
        nested_types={
            (AdvancedObjectTypes.OBJECT, frozenset({BasicNumericTypes.INTEGER}))
        },
    )

    assert description(language) == (
        "This test suite is not solvable in JavaScript, because JavaScript does "
        "not support object keys of type integer.\n"
        "Object keys in JavaScript can be: text."
    )


def test_map_keys_list_every_missing_type():
    language = make_language(
        Python,
        SupportedLanguage.PYTHON,
        nested_types={
            (
                BasicObjectTypes.MAP,
                frozenset({BasicSequenceTypes.SEQUENCE, BasicSequenceTypes.SET}),
            )
        },
    )

    assert description(language) == (
        "This test suite is not solvable in Python, because Python does not "
        "support map keys of types sequence and set.\n"
        "Map keys in Python can be: bigint, boolean, fixed_precision, integer, "
        "real, text, tuple."
    )


def test_constructs_of_expected_values_are_not_required():
    language = make_language(
        Haskell,
        SupportedLanguage.HASKELL,
        expected_values=FeatureSet(
            constructs={Construct.HETEROGENEOUS_COLLECTIONS},
            types=set(),
            nested_types=set(),
        ),
    )

    assert is_supported(language) is None


def test_types_of_expected_values_are_required():
    language = make_language(
        C,
        SupportedLanguage.C,
        expected_values=FeatureSet(
            constructs=set(),
            types={BasicSequenceTypes.SEQUENCE},
            nested_types=set(),
        ),
    )

    assert description(language) == (
        "This test suite is not solvable in C, because C does not support the "
        "type sequence."
    )


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
@pytest.mark.parametrize(
    "language, class_name", [("haskell", "Haskell"), ("python", "Python")]
)
def test_a_suite_with_an_optional_value_is_not_supported(
    suite_text: str,
    language: str,
    class_name: str,
    tmp_path: Path,
    pytestconfig: pytest.Config,
):
    suite = parse_json_suite(suite_text)
    conf = configuration(pytestconfig, "", language, tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)

    assert description(bundle.language) == (
        f"This test suite is not solvable in {class_name}, because {class_name} "
        "does not support:\n"
        "- optional values, such as a Haskell Maybe, written in the test suite"
    )


def test_mixed_list_in_expected_return_is_solvable_in_haskell():
    suite = parse_dsl(
        """
        - tab: "mixed"
          testcases:
            - expression: "pair_up(1)"
              return: ["a", 1]
        """
    )
    language = make_language(Haskell, SupportedLanguage.HASKELL)
    assert language.config is not None
    language.config.suite = suite

    assert is_supported(language) is None
