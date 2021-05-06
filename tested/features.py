"""
Module containing the definitions of the features we can support.
"""
import logging
import operator
from enum import Enum
from functools import reduce
from typing import Iterable, Set, NamedTuple, TYPE_CHECKING

from .datatypes import AllTypes, BasicSequenceTypes, BasicObjectTypes, NestedTypes
from tested.internal_timings import new_stage, end_stage

if TYPE_CHECKING:
    from .configs import Bundle

_logger = logging.getLogger(__name__)


class Construct(str, Enum):
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

Types = Set[AllTypes]
Constructs = Set[Construct]


class FeatureSet(NamedTuple):
    constructs: Constructs
    types: Types
    nested_types: NestedTypes


class WithFeatures:
    def get_used_features(self) -> FeatureSet:
        raise NotImplementedError


NOTHING = FeatureSet(constructs=set(), types=set(), nested_types=set())


def combine_features(iterable: Iterable[FeatureSet]) -> FeatureSet:
    """
    Combine multiple features into one features.
    """
    features = list(iterable)
    assert all(isinstance(x, FeatureSet) for x in features)
    constructs = reduce(
        operator.or_,
        (x.constructs for x in features),
        set()
    )
    types = reduce(
        operator.or_,
        (x.types for x in features),
        set()
    )
    nexted_types = reduce(
        operator.or_,
        (x.nested_types for x in features),
        set()
    )

    return FeatureSet(
        constructs=constructs,
        types=types,
        nested_types=nexted_types
    )


def is_supported(bundle: 'Bundle') -> bool:
    """
    Check if the given configuration bundle is supported. This will check if the
    testplan inside the bundle can be executed by the programming language in the
    bundle.

    :param bundle: The configuration bundle.

    :return: True or False
    """
    from .languages.config import TypeSupport

    new_stage("analyse.features", sub_stage=True)
    required = bundle.plan.get_used_features()
    end_stage("analyse.features", sub_stage=True)

    # Check constructs
    available_constructs = bundle.lang_config.supported_constructs()
    if not (required.constructs <= available_constructs):
        _logger.warning("This plan is not compatible!")
        _logger.warning(f"Required constructs are {required.constructs}.")
        _logger.warning(f"The language supports {available_constructs}.")
        missing = (required.constructs ^ available_constructs) & required.constructs
        _logger.warning(f"Missing features are: {missing}.")
        return False

    mapping = bundle.lang_config.type_support_map()
    for t in required.types:
        if mapping[t] == TypeSupport.UNSUPPORTED:
            _logger.warning(f"Plan requires unsupported type {t}")
            return False

    # Check language specific evaluators
    for tab in bundle.plan.tabs:
        for run in tab.runs:
            for context in run.contexts:
                for testcase in context.all_testcases():
                    languages = testcase.output.get_specific_eval_languages()
                    if languages is not None:
                        if bundle.config.programming_language not in languages:
                            _logger.warning(
                                f"Specific evaluators are available only in "
                                f"{languages}!"
                            )
                            return False

    nested_types = filter(lambda x: x[0] in (
        BasicSequenceTypes.SET, BasicObjectTypes.MAP), required.nested_types)
    restricted = bundle.lang_config.restriction_map()
    for key, value_types in nested_types:
        if not (value_types <= restricted[key]):
            _logger.warning("This plan is not compatible!")
            _logger.warning(f"Required {key} types are {value_types}.")
            _logger.warning(f"The language supports {restricted[key]}.")
            missing = (value_types ^ restricted[key]) & value_types
            _logger.warning(f"Missing types are: {missing}.")
            return False

    return True
