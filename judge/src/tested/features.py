"""
Module containing the definitions of the features we can support.
"""
import operator
from enum import Flag, auto
from functools import reduce
from typing import Iterable, Set, NamedTuple
from .datatypes import AllTypes


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


class WithFeatures:

    def get_used_features(self) -> FeatureSet:
        raise NotImplementedError()
