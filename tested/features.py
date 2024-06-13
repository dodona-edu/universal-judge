"""
Module containing the definitions of the features we can support.
"""

import logging
import operator
from collections import defaultdict
from collections.abc import Iterable
from enum import StrEnum, auto, unique
from functools import reduce
from typing import TYPE_CHECKING

from attrs import define

from tested.datatypes import (
    AllTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    ComplexExpressionTypes,
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


@define
class FeatureSet:
    constructs: set[Construct]
    types: set[AllTypes]
    nested_types: NestedTypes


class WithFeatures:
    def get_used_features(self) -> FeatureSet:
        raise NotImplementedError


NOTHING = FeatureSet(constructs=set(), types=set(), nested_types=set())


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


def fallback_type_support_map(language: "Language") -> dict[AllTypes, TypeSupport]:
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


def is_supported(language: "Language") -> list[Message] | None:
    """
    Check if the given configuration bundle is supported. This will check if the
    test suite inside the bundle can be executed by the programming language in the
    bundle.

    :param language: The configuration bundle.

    :return: True or False
    """

    assert language.config is not None
    required = language.config.suite.get_used_features()

    # Check constructs
    available_constructs = language.supported_constructs()
    if not (required.constructs <= available_constructs):
        _logger.warning("This test suite is not compatible!")
        _logger.warning(f"Required constructs are {required.constructs}.")
        _logger.warning(f"The language supports {available_constructs}.")
        missing = (required.constructs ^ available_constructs) & required.constructs
        _logger.warning(f"Missing features are: {missing}.")
        message_text = f"""
        This test suite is not solvable in {language.__class__.__name__}.
        
        The required language constructs are:
        {required.constructs}
        
        The programming language has support for:
        {available_constructs}
        
        This means support is lacking for these:
        {missing}
        
        Note that support for certain types means real support, not reduced support.
        For example, if tuples are required, the language must have a dedicated tuple
        type.
        
        One solution is to use less-specific types, e.g. using a sequence instead of
        a tuple.
        """
        return [ExtendedMessage(description=message_text, permission=Permission.STAFF)]

    mapping = fallback_type_support_map(language)
    for t in required.types:
        if mapping[t] == TypeSupport.UNSUPPORTED:
            _logger.warning(f"Test suite requires unsupported type {t}")
            message_text = f"""
            This test suite is not solvable in {language.__class__.__name__}.
            The test suite requires type {t}, which is not supported.
            """
            return [
                ExtendedMessage(description=message_text, permission=Permission.STAFF)
            ]

    # Check language-specific oracles
    for tab in language.config.suite.tabs:
        assert tab.contexts is not None
        for context in tab.contexts:
            for testcase in context.testcases:
                languages = testcase.get_specific_languages()
                if languages is not None:
                    if language.config.dodona.programming_language not in languages:
                        _logger.warning(
                            f"Language-specific constructs are available only in "
                            f"{languages}!"
                        )
                        message_text = f"""
                        This test suite is not solvable in {language.__class__.__name__}.
                        The test suite contains language-specific code, but code is only
                        provided for {languages}.
                        """
                        return [
                            ExtendedMessage(
                                description=message_text, permission=Permission.STAFF
                            )
                        ]

    nested_types = []
    for key, value_types in required.nested_types:
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
                basic_value_types.add(resolve_to_basic(value_type))
            else:
                basic_value_types.add(value_type)

        if not (basic_value_types <= restrictions):  # type: ignore
            _logger.warning("This test suite is not compatible!")
            _logger.warning(
                f"Required {key} types are {value_types} (or {basic_value_types})."
            )
            _logger.warning(f"The language supports {restrictions}.")
            missing = (value_types ^ restrictions) & value_types
            _logger.warning(f"Missing types are: {missing}.")
            message_text = f"""
            This test suite is not solvable in {language.__class__.__name__}.
            
            The required {key} types are:
            {value_types}
            
            The programming language has support for:
            {restrictions}
            
            This means support is lacking for these:
            {missing}
            
            Note that support for certain types means real support, not reduced support.
            For example, if tuples are required, the language must have a dedicated tuple
            type.
            
            One solution is to use less-specific types, e.g. using a sequence instead of
            a tuple.
            """
            return [
                ExtendedMessage(description=message_text, permission=Permission.STAFF)
            ]

    return None
