"""
Module containing the definitions of the features we can support.
"""

import logging
import operator
from collections import defaultdict
from collections.abc import Iterable, Sequence
from enum import StrEnum, auto, unique
from functools import reduce
from typing import TYPE_CHECKING

from attrs import define

from tested.datatypes import (
    AdvancedSequenceTypes,
    AllTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    ComplexExpressionTypes,
    ExpressionTypes,
    NestedTypes,
    resolve_to_basic,
)
from tested.dodona import ExtendedMessage, Message, Permission

if TYPE_CHECKING:
    from tested.languages.language import Language

_logger = logging.getLogger(__name__)


@unique
class Construct(StrEnum):
    # Object oriented stuff, classes, ...
    OBJECTS = "objects"
    EXCEPTIONS = "exceptions"
    # Main function. Does not necessarily imply FUNCTION_CALLS.
    FUNCTION_CALLS = "function_calls"
    ASSIGNMENTS = "assignments"

    HETEROGENEOUS_COLLECTIONS = "heterogeneous_collections"

    # Named arguments and default parameters are not necessarily related.
    # An argument can have a name but still be required.
    DEFAULT_PARAMETERS = "default_parameters"
    HETEROGENEOUS_ARGUMENTS = "heterogeneous_arguments"
    NAMED_ARGUMENTS = "named_arguments"

    # Programmed evaluation is possible in this language.
    EVALUATION = "evaluation"

    # Global variables
    GLOBAL_VARIABLES = "global_variables"

    # Only a submission returns optional values: no language supports them in a suite.
    OPTIONAL_VALUES = "optional_values"


@define
class FeatureSet:
    constructs: set[Construct]
    types: set[AllTypes]
    nested_types: NestedTypes


class WithFeatures:
    def get_used_features(self) -> FeatureSet:
        raise NotImplementedError


NOTHING = FeatureSet(constructs=set(), types=set(), nested_types=set())


@define
class SuiteFeatures:
    """
    The features a test suite uses, split by what TESTed does with them.
    """

    generated_code: FeatureSet
    """
    Used in the code TESTed generates in the language of the submission: the
    input and the harness that collects its output. The language has to support
    all of them.
    """
    expected_values: FeatureSet
    """
    Used in expected return values. TESTed compares these itself and only shows
    them in the language of the submission, so the language only has to support
    their types.
    """


class TypeSupport(StrEnum):
    SUPPORTED = auto()
    """
    The type is fully supported.
    
    For advanced types, this requires the language to have a suitable, distinct
    type. It is not enough that another type can support it. For example, Python
    does not have support for int16, even though all int16 values can easily fit
    into the Python integer type.
    """
    UNSUPPORTED = auto()
    """
    There is no support. This is the default value to allow for expansion of the
    types. Exercises which use these types will not be solvable in a language
    for which the type is unsupported.
    """
    REDUCED = auto()
    """
    Used for advanced types only. This means the language has no support for the
    type with a distinct type, but there is support using other types. In this
    case, exercises using this type are still solvable in the programming language.
    TESTed will use the basic type in those languages.
    """


def fallback_type_support_map(
    language: Language,
) -> dict[ExpressionTypes, TypeSupport]:
    """
    Return a map containing the support for all types.

    See the documentation on the TypeSupport enum for information on how
    to interpret the results.

    Note that it is considered a programming error if a basic type is not
    supported, but the advanced type is supported. This requirement is
    checked by TESTed when using the language.

    :return: The typing support dict.
    """
    config = defaultdict(lambda: TypeSupport.UNSUPPORTED)
    for x, y in language.datatype_support().items():
        if isinstance(y, str):
            config[x] = TypeSupport[y.upper()]
        else:
            config[x] = y
    return config


def combine_features(iterable: Iterable[FeatureSet]) -> FeatureSet:
    """
    Combine multiple features into one features.
    """
    features = list(iterable)
    assert all(isinstance(x, FeatureSet) for x in features)
    # noinspection PyTypeChecker
    constructs = reduce(operator.or_, (x.constructs for x in features), set())
    # noinspection PyTypeChecker
    types = reduce(operator.or_, (x.types for x in features), set())
    # noinspection PyTypeChecker
    nexted_types = reduce(operator.or_, (x.nested_types for x in features), set())

    return FeatureSet(constructs=constructs, types=types, nested_types=nexted_types)


def combine_suite_features(iterable: Iterable[SuiteFeatures]) -> SuiteFeatures:
    """
    Combine multiple suite features into one, keeping the split.
    """
    features = list(iterable)
    return SuiteFeatures(
        generated_code=combine_features(x.generated_code for x in features),
        expected_values=combine_features(x.expected_values for x in features),
    )


_CONSTRUCT_DESCRIPTIONS: dict[Construct, str] = {
    Construct.OBJECTS: "objects: constructors, methods or properties",
    Construct.EXCEPTIONS: "expected exceptions",
    Construct.FUNCTION_CALLS: "function calls",
    Construct.ASSIGNMENTS: "assigning a value to a variable",
    Construct.HETEROGENEOUS_COLLECTIONS: (
        'a collection with elements of different types, such as ["a", 1]'
    ),
    Construct.DEFAULT_PARAMETERS: (
        "calling the same function with different numbers of arguments, "
        "such as f(1) and f(1, 2)"
    ),
    Construct.HETEROGENEOUS_ARGUMENTS: (
        "calling the same function with arguments of different types in the "
        'same position, such as f(1) and f("a")'
    ),
    Construct.NAMED_ARGUMENTS: 'named arguments, such as f(name="a")',
    Construct.EVALUATION: "programmed evaluation",
    Construct.GLOBAL_VARIABLES: "reading a global variable",
    Construct.OPTIONAL_VALUES: "optional values, such as a Haskell Maybe, written in the test suite",
}

_CONSTRUCT_ADVICE: dict[Construct, str] = {
    Construct.HETEROGENEOUS_COLLECTIONS: (
        'If the values form a fixed group, write them as a tuple: ("a", 1).'
    ),
    Construct.NAMED_ARGUMENTS: "Pass the arguments by position instead.",
}


def _join(names: Sequence[str]) -> str:
    """
    Join names as "a", "a and b" or "a, b and c".
    """
    if len(names) <= 1:
        return "".join(names)
    return f"{', '.join(names[:-1])} and {names[-1]}"


def _educator_message(*lines: str) -> list[Message]:
    return [
        ExtendedMessage(description="\n".join(lines), permission=Permission.EDUCATOR)
    ]


def is_supported(language: Language) -> list[Message] | None:
    """
    Check if the given configuration bundle is supported. This will check if the
    test suite inside the bundle can be executed by the programming language in the
    bundle.

    :param language: The configuration bundle.

    :return: None if supported, otherwise a single educator message for the first
        failing check.
    """

    assert language.config is not None
    name = language.__class__.__name__
    features = language.config.suite.get_suite_features()
    # TESTed cannot compare an expected value that holds an optional.
    required_constructs = features.generated_code.constructs | (
        features.expected_values.constructs & {Construct.OPTIONAL_VALUES}
    )
    required_types = features.generated_code.types | features.expected_values.types
    required_nested_types = (
        features.generated_code.nested_types | features.expected_values.nested_types
    )
    mapping = fallback_type_support_map(language)

    # Check constructs
    available_constructs = language.supported_constructs()
    if not (required_constructs <= available_constructs):
        missing = required_constructs - available_constructs
        _logger.warning(f"Test suite requires unsupported constructs {missing}.")
        lines = [
            f"This test suite is not solvable in {name}, because {name} does not support:"
        ]
        for construct in Construct:
            if construct not in missing:
                continue
            lines.append(f"- {_CONSTRUCT_DESCRIPTIONS[construct]}")
            advice = _CONSTRUCT_ADVICE.get(construct)
            if (
                construct == Construct.HETEROGENEOUS_COLLECTIONS
                and mapping[AdvancedSequenceTypes.TUPLE] != TypeSupport.SUPPORTED
            ):
                advice = None
            if advice is not None:
                lines.append(f"  {advice}")
        return _educator_message(*lines)

    unsupported = sorted(
        (t for t in required_types if mapping[t] == TypeSupport.UNSUPPORTED), key=str
    )
    if unsupported:
        _logger.warning(f"Test suite requires unsupported types {unsupported}")
        names = [str(t) for t in unsupported]
        plural = "types" if len(names) > 1 else "type"
        lines = [
            (
                f"This test suite is not solvable in {name}, because {name} does "
                f"not support the {plural} {_join(names)}."
            )
        ]
        for t in unsupported:
            basic = resolve_to_basic(t)
            if mapping[basic] != TypeSupport.UNSUPPORTED:
                lines.append(
                    f"{name} does support the less specific type {basic} instead of {t}."
                )
        return _educator_message(*lines)

    # Check language-specific oracles
    dodona_language = language.config.dodona.programming_language
    for tab in language.config.suite.tabs:
        assert tab.contexts is not None
        for context in tab.contexts:
            for testcase in context.testcases:
                languages = testcase.get_specific_languages()
                if languages is not None and dodona_language not in languages:
                    _logger.warning(
                        f"Language-specific constructs are available only in "
                        f"{languages}!"
                    )
                    joined = _join(sorted(str(x) for x in languages))
                    return _educator_message(
                        f"This test suite is not solvable in {name}, because its "
                        f"language-specific code exists only for {joined}.",
                        f"Add a {dodona_language} version to make it solvable in "
                        f"{name}.",
                    )

    nested_types = []
    for key, value_types in required_nested_types:
        if isinstance(key, ComplexExpressionTypes):
            continue
        basic_key = resolve_to_basic(key)
        if basic_key in (BasicSequenceTypes.SET, BasicObjectTypes.MAP):
            nested_types.append((key, value_types))

    collection_restrictions = language.collection_restrictions()

    for key, value_types in nested_types:
        basic_key = resolve_to_basic(key)
        restrictions = collection_restrictions.get(
            key, collection_restrictions.get(basic_key)
        )

        # If None, skip as there are no restrictions.
        if restrictions is None:
            continue

        # Types that have reduced support are converted, so we also need to
        # convert them here in the checks.
        basic_value_types = set()
        for value_type in value_types:
            if mapping[value_type] == TypeSupport.REDUCED:
                assert not isinstance(value_type, ComplexExpressionTypes)
                basic_value_types.add(resolve_to_basic(value_type))
            else:
                basic_value_types.add(value_type)

        if not (basic_value_types <= restrictions):
            missing = sorted(basic_value_types - restrictions, key=str)
            _logger.warning(f"Required {key} types {missing} are not supported.")
            if resolve_to_basic(key) == BasicSequenceTypes.SET:
                kind = f"{key} elements"
            else:
                kind = f"{key} keys"
            missing_names = [str(t) for t in missing]
            plural = "types" if len(missing_names) > 1 else "type"
            allowed = sorted(
                str(t)
                for t in restrictions
                if not isinstance(t, ComplexExpressionTypes)
            )
            return _educator_message(
                f"This test suite is not solvable in {name}, because {name} does "
                f"not support {kind} of {plural} {_join(missing_names)}.",
                f"{kind.capitalize()} in {name} can be: {', '.join(allowed)}.",
            )

    return None
