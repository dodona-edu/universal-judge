"""
Module containing the definitions of the features we can support.
"""
from enum import Flag, auto
from functools import reduce
from typing import Iterable


class Features(Flag):
    # No special features are used
    NOTHING = 0
    # Language features
    OBJECTS = auto()  # Object oriented stuff, classes, ...
    EXCEPTIONS = auto()
    MAIN = auto()  # Main function. Does not necessarily imply FUNCTION_CALL.
    FUNCTION_CALL = auto()
    ASSIGNMENT = auto()
    # Datastructures
    LISTS = auto()
    SETS = auto()
    MAPS = auto()
    INTEGERS = auto()
    RATIONALS = auto()
    STRINGS = auto()
    BOOLEANS = auto()
    NULL = auto()


def reduce_features(iterable: Iterable[Features]) -> Features:
    """
    Reduce a list of features to a single feature. This will "OR" all features in
    the iterable.
    :param iterable: The list of features.
    :return: The combined features.
    """
    return reduce(lambda f1, f2: f1 | f2, iterable, Features.NOTHING)
