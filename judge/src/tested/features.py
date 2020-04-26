"""
Module containing the definitions of the features we can support.
"""
import logging
import operator
from enum import Enum
from functools import reduce
from typing import Iterable, Set, NamedTuple, TYPE_CHECKING

from .datatypes import AllTypes

if TYPE_CHECKING:
    from .configs import Bundle

_logger = logging.getLogger(__name__)


class Construct(str, Enum):
    # Object oriented stuff, classes, ...
    OBJECTS = "objects"
    EXCEPTIONS = "exceptions"
    # Main function. Does not necessarily imply FUNCTION_CALL.
    MAIN = "main"
    FUNCTION_CALL = "function_calls"
    ASSIGNMENT = "assignments"

    HETEROGENEOUS_COLLECTIONS = "heterogeneous_collections"

    DEFAULT_ARGUMENTS = "default_arguments"
    HETEROGENEOUS_ARGUMENTS = "heterogeneous_arguments"

    # Programmed evaluation is possible in this language.
    EVALUATION = "evaluation"


Types = Set[AllTypes]
Constructs = Set[Construct]


class FeatureSet(NamedTuple):
    constructs: Constructs
    types: Types


class WithFeatures:
    def get_used_features(self) -> FeatureSet:
        raise NotImplementedError


NOTHING = FeatureSet(constructs=set(), types=set())


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

    return FeatureSet(
        constructs=constructs,
        types=types
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

    required = bundle.plan.get_used_features()

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
        for context in tab.contexts:
            for testcase in context.all_testcases():
                languages = testcase.output.get_specific_eval_languages()
                if languages is not None:
                    if bundle.config.programming_language not in languages:
                        _logger.warning(
                            f"Specific evaluators are available only in "
                            f"{languages}!"
                        )
                        return False

    return True
