"""
Module containing the definitions of the features we can support.
"""
import operator
from enum import Flag, auto
from functools import reduce
from typing import Iterable, Set, NamedTuple, TYPE_CHECKING

from .datatypes import AllTypes
if TYPE_CHECKING:
    from .configs import Bundle


class Constructs(Flag):
    # No special features are used
    NOTHING = 0
    # Language features
    OBJECTS = auto()  # Object oriented stuff, classes, ...
    EXCEPTIONS = auto()
    MAIN = auto()  # Main function. Does not necessarily imply FUNCTION_CALL.
    FUNCTION_CALL = auto()
    ASSIGNMENT = auto()

    ALL = OBJECTS | EXCEPTIONS | MAIN | FUNCTION_CALL | ASSIGNMENT


Types = Set[AllTypes]


class FeatureSet(NamedTuple):
    constructs: Constructs
    types: Types


class WithFeatures:
    def get_used_features(self) -> FeatureSet:
        raise NotImplementedError()


NOTHING = FeatureSet(constructs=Constructs.NOTHING, types=set())


def combine_features(iterable: Iterable[FeatureSet]) -> FeatureSet:
    """
    Combine multiple features into one features.
    """
    constructs = reduce(
        operator.or_,
        (x.constructs for x in iterable),
        Constructs.NOTHING
    )
    types = reduce(
        operator.or_,
        (x.types for x in iterable),
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
    required = bundle.plan.get_used_features()

    # Check constructs
    available_constructs = bundle.language_config.supported_constructs()
    if required.constructs & available_constructs != required.constructs:
        return False

    type_mapping = bundle.language_config.type_support_map()
    # Check types
    supported = True
    for required_type in required.types:
        if required_type in type_mapping:
            # If the required type is in the map, it is only valid if it is not
            # mapped to none.
            supported = supported and type_mapping[required_type] is not None
        else:
            # If the required type is not in the map, it is OK.
            pass

    return supported
