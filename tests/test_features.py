"""
Tests for the features used by expressions and statements, which determine which
programming languages can run a test suite.
"""

from tested.datatypes import BasicSequenceTypes, ComplexExpressionTypes
from tested.dsl.ast_translator import parse_string
from tested.features import Construct


def _features(code: str):
    return parse_string(code).get_used_features()


def test_function_call_on_submission():
    features = _features("foo()")
    assert Construct.FUNCTION_CALLS in features.constructs
    assert Construct.OBJECTS not in features.constructs


def test_method_call_on_identifier_requires_objects():
    # On HEAD, a method call on a plain identifier did not require objects.
    features = _features("a.foo()")
    assert Construct.FUNCTION_CALLS in features.constructs
    assert Construct.OBJECTS in features.constructs


def test_constructor_requires_objects():
    assert Construct.OBJECTS in _features("Foo()").constructs


def test_submission_global_requires_global_variables():
    for code in ["X", "submission.x"]:
        assert Construct.GLOBAL_VARIABLES in _features(code).constructs


def test_suite_variable_is_an_identifier():
    features = _features("x")
    assert features.constructs == set()
    assert (ComplexExpressionTypes.IDENTIFIERS, frozenset()) in features.nested_types


def test_identifier_in_collection_is_a_nested_type():
    features = _features("{x}")
    assert (
        BasicSequenceTypes.SET,
        frozenset({ComplexExpressionTypes.IDENTIFIERS}),
    ) in features.nested_types


def test_property_read_requires_objects_but_not_assignments():
    features = _features("a.x")
    assert Construct.OBJECTS in features.constructs
    assert Construct.ASSIGNMENTS not in features.constructs


def test_receiver_features_are_included():
    # On HEAD, the features of the receiver (here: a set) were ignored.
    features = _features("f({1}).g()")
    assert BasicSequenceTypes.SET in features.types


def test_property_assignment_includes_target_features():
    features = _features("f({1}).y = 5")
    assert Construct.ASSIGNMENTS in features.constructs
    assert Construct.OBJECTS in features.constructs
    assert BasicSequenceTypes.SET in features.types
