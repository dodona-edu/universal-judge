"""
Module containing the definitions of the features we can support.
"""
import logging
import operator
from enum import Flag, auto
from functools import reduce
from typing import Iterable, Set, NamedTuple, TYPE_CHECKING

from .datatypes import AllTypes

if TYPE_CHECKING:
    from .configs import Bundle

_logger = logging.getLogger(__name__)


class Constructs(Flag):
    # No special features are used
    NOTHING = 0
    # Language features
    OBJECTS = auto()  # Object oriented stuff, classes, ...
    EXCEPTIONS = auto()
    MAIN = auto()  # Main function. Does not necessarily imply FUNCTION_CALL.
    FUNCTION_CALL = auto()
    ASSIGNMENT = auto()

    HETEROGENEOUS_COLLECTIONS = auto()

    DEFAULT_ARGUMENTS = auto()
    HETEROGENEOUS_ARGUMENTS = auto()

    EVALUATION = auto()  # Programmed evaluation is possible in this language.

    ALL = (OBJECTS | EXCEPTIONS | MAIN | FUNCTION_CALL | ASSIGNMENT
           | HETEROGENEOUS_COLLECTIONS | DEFAULT_ARGUMENTS
           | HETEROGENEOUS_ARGUMENTS | EVALUATION)


Types = Set[AllTypes]


class FeatureSet(NamedTuple):
    constructs: Constructs
    types: Types


class WithFeatures:
    def get_used_features(self) -> FeatureSet:
        raise NotImplementedError


NOTHING = FeatureSet(constructs=Constructs.NOTHING, types=set())


def combine_features(iterable: Iterable[FeatureSet]) -> FeatureSet:
    """
    Combine multiple features into one features.
    """
    features = list(iterable)
    assert all(isinstance(x, FeatureSet) for x in features)
    constructs = reduce(
        operator.or_,
        (x.constructs for x in features),
        Constructs.NOTHING
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
    available_constructs = bundle.language_config.supported_constructs()
    if (required.constructs & available_constructs) != required.constructs:
        _logger.warning("This plan is not compatible!")
        _logger.warning(f"Required constructs are {required.constructs}.")
        _logger.warning(f"The language supports {available_constructs}.")
        missing = (required.constructs ^ available_constructs) & required.constructs
        _logger.warning(f"Missing features are: {missing}.")
        return False

    mapping = bundle.language_config.type_support_map()
    for t in required.types:
        if mapping[t] == TypeSupport.UNSUPPORTED:
            _logger.warning(f"Plan requires unsupported type {t}")
            return False
    return True
