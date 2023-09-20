"""
The serialization format.

This is the authoritative source of the format. For example, json-schema is
generated from this code.

This module does not concern itself with actual decoding or encoding; it is purely
concerned about the format itself and parsing of the format itself (some might
expression
this the meta-concerns).

The encoding and decoding of instances of values in this format are done in the
language implementations itself.

A json-schema can be generated from this format by executing the module on the
command line. The schema will be printed to stdout. This can be used to generate
classes for implementations in other configs.
"""
import copy
import logging
import math
import operator
from collections.abc import Iterable
from decimal import Decimal
from enum import StrEnum, auto, unique
from functools import reduce
from types import NoneType
from typing import Any, Literal, Optional, Union, cast

from attrs import define, field, resolve_types, validators

from tested.datatypes import (
    AdvancedNumericTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    BooleanTypes,
    ComplexExpressionTypes,
    ExpressionTypes,
    NestedTypes,
    NothingTypes,
    NumericTypes,
    ObjectTypes,
    SequenceTypes,
    SimpleTypes,
    StringTypes,
    resolve_to_basic,
)
from tested.features import Construct, FeatureSet, WithFeatures, combine_features
from tested.parsing import parse_json_value
from tested.utils import flatten, sorted_no_duplicates

logger = logging.getLogger(__name__)

WrappedAllTypes = Union[
    AllTypes,
    tuple[SequenceTypes, "WrappedAllTypes"],
    tuple[ObjectTypes, tuple["WrappedAllTypes", "WrappedAllTypes"]],
]


def _get_type_for(expression: "Expression") -> WrappedAllTypes:
    if isinstance(expression, SequenceType):
        return expression.type, expression.get_content_type()
    elif isinstance(expression, ObjectType):
        return expression.type, (expression.get_key_type(), expression.get_value_type())
    elif isinstance(expression, Value):
        return expression.type
    else:
        return BasicStringTypes.ANY


def _get_combined_types(types: Iterable[WrappedAllTypes]) -> WrappedAllTypes:
    type_dict = dict()
    for data_type in types:
        if isinstance(data_type, tuple):
            try:
                type_item = type_dict[data_type[0]]
                if isinstance(type_item, tuple):
                    if isinstance(data_type[1], tuple):
                        type_dict[data_type[0]][0].add(data_type[1][0])
                        type_dict[data_type[0]][1].add(data_type[1][1])
                    else:
                        type_dict[data_type[0]] = {BasicStringTypes.ANY}
                else:
                    assert isinstance(type_item, set)
                    if isinstance(data_type[1], tuple):
                        type_item.add(BasicStringTypes.ANY)
                    else:
                        type_item.add(data_type[1])
            except KeyError:
                if isinstance(data_type[1], tuple):
                    type_dict[data_type[0]] = ({data_type[1][0]}, {data_type[1][1]})
                else:
                    type_dict[data_type[0]] = {data_type[1]}
        else:
            type_dict[data_type] = None
    if len(type_dict) == 1:
        key_type, sub_type = next(iter(type_dict.items()))
        if sub_type is None:
            return key_type
        elif isinstance(sub_type, tuple):
            return key_type, (
                _get_combined_types(sub_type[0]),
                _get_combined_types(sub_type[1]),
            )
        else:
            return key_type, _get_combined_types(sub_type)
    else:
        return BasicStringTypes.ANY


def _combine_nested_types(
    data_type: AllTypes, features: Iterable[FeatureSet]
) -> NestedTypes:
    nested_types: Iterable[ExpressionTypes] = {
        y[0] for f in features for y in f.nested_types
    }
    # noinspection PyTypeChecker
    return {
        (
            data_type,
            frozenset(
                reduce(
                    operator.or_,
                    (x[1] for f in features for x in f.nested_types),
                    nested_types,
                )
            ),
        )
    }


def _get_self_nested_type(data_type: AllTypes) -> NestedTypes:
    return {(data_type, frozenset())}


class WithFunctions:
    def get_functions(self) -> Iterable["FunctionCall"]:
        raise NotImplementedError


@unique
class SpecialNumbers(StrEnum):
    NOT_A_NUMBER = "nan"
    POS_INFINITY = "inf"
    NEG_INFINITY = "-inf"


@define
class NumberType(WithFeatures, WithFunctions):
    type: NumericTypes = field(validator=validators.instance_of(NumericTypes))
    data: SpecialNumbers | int | float | Decimal = field(
        validator=validators.instance_of(Union[SpecialNumbers, int, float, Decimal])
    )
    diagnostic: Literal[None] = None  # Unused in this type.

    def __attrs_post_init__(self):
        if (
            isinstance(self.data, SpecialNumbers)
            and resolve_to_basic(self.type) == BasicNumericTypes.INTEGER
        ):
            raise ValueError(
                f"SpecialNumber '{self.data}' is only supported for " f"real numbers."
            )

        if resolve_to_basic(self.type) == BasicNumericTypes.INTEGER and isinstance(
            self.data, Decimal
        ):
            self.data = self.data.to_integral_value()

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), {self.type}, _get_self_nested_type(self.type))

    def get_functions(self) -> Iterable["FunctionCall"]:
        return []


@define
class StringType(WithFeatures, WithFunctions):
    type: StringTypes = field(validator=validators.instance_of(StringTypes))
    data: str = field(validator=validators.instance_of(str))

    # Optional string representation of the type of the value, if the type is
    # "unknown". TESTed will not do anything with this, as the actual type is
    # unknown, but it will be shown to the user to aid them in debugging their
    # error.
    diagnostic: str | None = None

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), {self.type}, _get_self_nested_type(self.type))

    def get_functions(self) -> Iterable["FunctionCall"]:
        return []


@define
class BooleanType(WithFeatures, WithFunctions):
    type: BooleanTypes = field(validator=validators.instance_of(BooleanTypes))
    data: bool = field(validator=validators.instance_of(bool))
    diagnostic: Literal[None] = None  # Unused in this type.

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), {self.type}, _get_self_nested_type(self.type))

    def get_functions(self) -> Iterable["FunctionCall"]:
        return []


@define
class SequenceType(WithFeatures, WithFunctions):
    type: SequenceTypes = field(validator=validators.instance_of(SequenceTypes))
    data: list["Expression"]
    diagnostic: Literal[None] = None  # Unused in this type.

    def get_used_features(self) -> FeatureSet:
        nested_features = [x.get_used_features() for x in self.data]
        combined_nested_types = _combine_nested_types(self.type, nested_features)
        base_features = FeatureSet(set(), {self.type}, combined_nested_types)
        combined = combine_features([base_features] + nested_features)
        content_type = self.get_content_type()
        if content_type == BasicStringTypes.ANY:
            combined = combine_features(
                [
                    FeatureSet(
                        {Construct.HETEROGENEOUS_COLLECTIONS},
                        {self.type},
                        combined_nested_types,
                    )
                ]
                + nested_features
            )
        return combined

    def get_content_type(self) -> WrappedAllTypes:
        """
        Attempt to get a type for the content of the container. The function will
        attempt to get the most specific type possible.
        """
        return _get_combined_types(_get_type_for(element) for element in self.data)

    def get_functions(self) -> Iterable["FunctionCall"]:
        return flatten(x.get_functions() for x in self.data)


@define
class ObjectKeyValuePair(WithFeatures, WithFunctions):
    key: "Expression"
    value: "Expression"

    def get_key_type(self) -> WrappedAllTypes:
        """
        Attempt to get a type for the key of the key-value pair. The function will
        attempt to get the most specific type possible.
        """
        return _get_type_for(self.key)

    def get_value_type(self) -> WrappedAllTypes:
        """
        Attempt to get a type for the value of the key-value pair. The function will
        attempt to get the most specific type possible.
        """
        return _get_type_for(self.value)

    def get_used_features(self) -> FeatureSet:
        nested_features = [self.key.get_used_features(), self.value.get_used_features()]
        return combine_features(nested_features)

    def get_functions(self) -> Iterable["FunctionCall"]:
        return self.value.get_functions()


@define
class ObjectType(WithFeatures, WithFunctions):
    type: ObjectTypes = field(validator=validators.instance_of(ObjectTypes))
    data: list[ObjectKeyValuePair]
    diagnostic: Literal[None] = None  # Unused in this type.

    def get_key_type(self) -> WrappedAllTypes:
        """
        Attempt to get a type for the keys of the object. The function will
        attempt to get the most specific type possible.
        """
        return _get_combined_types(element.get_key_type() for element in self.data)

    def get_value_type(self) -> WrappedAllTypes:
        """
        Attempt to get a type for the values of the object. The function will
        attempt to get the most specific type possible.
        """
        return _get_combined_types(element.get_value_type() for element in self.data)

    def get_used_features(self) -> FeatureSet:
        key_nested_features = [x.key.get_used_features() for x in self.data]
        key_combined = _combine_nested_types(self.type, key_nested_features)
        base_features = FeatureSet(set(), {self.type}, key_combined)
        value_nested_features = [x.value.get_used_features() for x in self.data]
        return combine_features(
            [base_features] + value_nested_features + key_nested_features
        )

    def get_functions(self) -> Iterable["FunctionCall"]:
        return flatten(
            flatten((pair.key.get_functions(), pair.value.get_functions()))
            for pair in self.data
        )


@define
class NothingType(WithFeatures, WithFunctions):
    type: NothingTypes = field(
        default=BasicNothingTypes.NOTHING,
        validator=validators.instance_of(NothingTypes),  # type: ignore
    )
    data: Literal[None] = None
    diagnostic: Literal[None] = None  # Unused in this type.

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), {self.type}, _get_self_nested_type(self.type))

    def get_functions(self) -> Iterable["FunctionCall"]:
        return []


# A value is one of the preceding types.
Value = Union[
    NumberType, StringType, BooleanType, SequenceType, ObjectType, NothingType
]


class Identifier(str, WithFeatures, WithFunctions):
    """Represents an variable name."""

    is_raw: bool

    def __new__(cls, *args, **kwargs):
        the_class = str.__new__(cls, *args, **kwargs)
        the_class.is_raw = False
        return the_class

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(
            set(), set(), {(ComplexExpressionTypes.IDENTIFIERS, frozenset())}
        )

    def get_functions(self) -> Iterable["FunctionCall"]:
        return []


@unique
class FunctionType(StrEnum):
    FUNCTION = auto()
    """
    A function expression. A function can be in a namespace if it is provided.
    The namespace can be an instance, in which case it is a method (e.g. Java or
    Python), but it can also be a function inside a module (e.g. Haskell).
    For example, in Java, a function without a given namespace will have the
    namespace of its implementing class
    """
    CONSTRUCTOR = auto()
    """
    A constructor.
    """
    PROPERTY = auto()
    """
    Access a property on an object.
    """


@define
class NamedArgument(WithFeatures, WithFunctions):
    """Represents a named argument for a function."""

    name: str
    value: "Expression"

    def get_used_features(self) -> FeatureSet:
        this = FeatureSet(
            constructs={Construct.NAMED_ARGUMENTS}, types=set(), nested_types=set()
        )
        expression = self.value.get_used_features()
        return combine_features([this, expression])

    def get_functions(self) -> Iterable["FunctionCall"]:
        return self.value.get_functions()


@define
class FunctionCall(WithFeatures, WithFunctions):
    """
    Represents a function expression.
    """

    type: FunctionType
    name: str
    namespace: Optional["Expression"] = None
    arguments: list[Union[NamedArgument, "Expression"]] = field(factory=list)

    def __attrs_post_init__(self):
        if self.type == FunctionType.PROPERTY and self.arguments:
            raise ValueError("You cannot have arguments for a property!")

    def get_used_features(self) -> FeatureSet:
        if self.type == FunctionType.PROPERTY and self.namespace is None:
            return FeatureSet({Construct.GLOBAL_VARIABLES}, set(), set())

        constructs = {Construct.FUNCTION_CALLS}

        # Get OOP features.
        if self.type in (
            FunctionType.PROPERTY,
            FunctionType.CONSTRUCTOR,
        ) or not isinstance(self.namespace, (Identifier, NoneType)):
            constructs.add(Construct.OBJECTS)

        base_features = FeatureSet(
            constructs=constructs,
            types=set(),
            nested_types={(ComplexExpressionTypes.FUNCTION_CALLS, frozenset())},
        )
        argument_features = [x.get_used_features() for x in self.arguments]

        return combine_features([base_features] + argument_features)

    def get_functions(self) -> Iterable["FunctionCall"]:
        if self.namespace is None:
            namespace_nested = []
        else:
            namespace_nested = [list(self.namespace.get_functions())]
        return flatten(
            [
                [self],
                *[list(x.get_functions()) for x in self.arguments],
                *namespace_nested,
            ]
        )


@define
class VariableType:
    data: str
    type: Literal["custom"] = "custom"


Expression = Identifier | Value | FunctionCall


@define
class Assignment(WithFeatures, WithFunctions):
    """
    Assigns the return value of a function to a variable. Because the expression
    part is pretty simple, the type of the value is determined by looking at the
    expression. It is also possible to define the type. If the type cannot be
    determined and it is not specified, this is an error.
    """

    variable: str
    expression: Expression
    type: AllTypes | VariableType

    def replace_expression(self, expression: Expression) -> "Assignment":
        return Assignment(variable=self.variable, expression=expression, type=self.type)

    def replace_variable(self, variable: str) -> "Assignment":
        return Assignment(variable=variable, expression=self.expression, type=self.type)

    def replace_type(self, type_name: AllTypes | VariableType) -> "Assignment":
        return Assignment(
            variable=self.variable, expression=self.expression, type=type_name
        )

    def get_used_features(self) -> FeatureSet:
        base = FeatureSet({Construct.ASSIGNMENTS}, set(), set())
        other = self.expression.get_used_features()

        return combine_features([base, other])

    def get_functions(self) -> Iterable["FunctionCall"]:
        return self.expression.get_functions()


# If changing this, also update is_statement_strict in the utils.
Statement = Assignment | Expression

# Update the forward references, which fixes the schema generation.
resolve_types(ObjectType)
resolve_types(SequenceType)
resolve_types(NamedArgument)
resolve_types(FunctionCall)
resolve_types(ObjectKeyValuePair)


def as_basic_type(value: Value) -> Value:
    """Convert a value's type to a basic type."""
    new_type = resolve_to_basic(value.type)
    cp = copy.copy(value)
    cp.type = new_type  # type: ignore
    return cp


def parse_value(value: str) -> Value:
    """
    Parse the json of a value into the relevant data structures.

    If ``value`` is not valid json, a `SerialisationError` will be thrown.

    :param value: The json to be parsed.
    :return: The parsed data.
    """

    return parse_json_value(value)


class PrintingDecimal:
    def __init__(self, decimal: Decimal):
        self.decimal = decimal

    def __repr__(self):
        return str(self.decimal)


def _convert_to_python(value: Value | None, for_printing=False) -> Any:
    """
    Convert the parsed values into the proper Python type. This is basically
    the same as deserialising a value, but this function is currently not re-used
    in the Python implementation, since run-time de-serialisation is not supported.
    :param value: The parsed value.
    :param for_printing: If the result will be used for printing or not.
    :return: The Python value.
    """
    if value is None:
        return None

    # If we have a type for which the data is usable in Python, use it.
    if isinstance(value.type, SimpleTypes):
        # If we have floats or ints, convert them to Python.
        if value.type in (
            AdvancedNumericTypes.SINGLE_PRECISION,
            AdvancedNumericTypes.DOUBLE_PRECISION,
        ):
            return float(str(value.data))
        if value.type != AdvancedNumericTypes.FIXED_PRECISION:
            return int(str(value.data))
        if for_printing:
            assert isinstance(value.data, Decimal)
            return PrintingDecimal(value.data)

        return value.data

    if isinstance(value.type, SequenceTypes):
        assert isinstance(value, SequenceType)
        values = [_convert_to_python(cast(Value, x)) for x in value.data]
        basic_type = resolve_to_basic(value.type)
        if basic_type == BasicSequenceTypes.SEQUENCE:
            return values
        if basic_type == BasicSequenceTypes.SET:
            return sorted_no_duplicates(
                _convert_to_python(cast(Value, x)) for x in value.data
            )
        raise AssertionError(f"Unknown basic sequence type {basic_type}.")

    if isinstance(value.type, ObjectTypes):
        assert isinstance(value, ObjectType)
        return sorted_no_duplicates(
            (
                (
                    _convert_to_python(cast(Value, pair.key)),
                    _convert_to_python(cast(Value, pair.value)),
                )
                for pair in value.data
            ),
            key=lambda x: x[0],
        )

    if isinstance(value, NothingType):
        return None

    # Unknown type.
    logger.warning(f"Unknown data type {value.type} will be interpreted as string.")
    return str(value.data)


def serialize_from_python(value: Any, type_: AllTypes | None = None) -> Value:
    """
    Convert a (simple) Python value into a TESTed value.

    While unfortunate, this should be kept in sync with the
    JSON encoder for values of the language module for Python.
    """
    if value is None:
        assert isinstance(type_, NothingTypes | None)
        return NothingType(type=type_ or BasicNothingTypes.NOTHING)
    elif isinstance(value, str):
        assert isinstance(type_, StringTypes | None)
        return StringType(type=type_ or BasicStringTypes.TEXT, data=value)
    elif isinstance(value, bool):
        assert isinstance(type_, BooleanTypes | None)
        return BooleanType(type=type_ or BasicBooleanTypes.BOOLEAN, data=value)
    elif isinstance(value, int):
        assert isinstance(type_, NumericTypes | None)
        return NumberType(type=type_ or BasicNumericTypes.INTEGER, data=value)
    elif isinstance(value, float):
        assert isinstance(type_, NumericTypes | None)
        return NumberType(type=type_ or BasicNumericTypes.REAL, data=value)
    else:
        raise TypeError(f"No clue how to convert {value} into TESTed value.")


class ComparableFloat:
    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        # noinspection PyBroadException
        try:
            if math.isnan(self.value) and math.isnan(other.value):
                return True
            return math.isclose(self.value, other.value)
        except Exception:
            return False

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return repr(self.value)

    def __bool__(self):
        return bool(self.value)


def to_python_comparable(value: Value | None) -> Any:
    """
    Convert the value into a comparable Python value. Most values are just converted
    to their built-in Python variant. Some, however, are not. For example, floats
    are converted into a wrapper class, which allows for comparison.

    This does mean that the type of the returned value can differ from the type in
    the return channel (in the test suite). The returned value is only guaranteed
    to support the following operations: eq, str, repr and bool.
    """
    if value is None:
        return None
    basic_type = resolve_to_basic(value.type)
    if value.type == AdvancedNumericTypes.FIXED_PRECISION:
        assert isinstance(value.data, Decimal)
        return Decimal(value.data)
    if basic_type == BasicSequenceTypes.SEQUENCE:
        assert isinstance(value, SequenceType)
        return [to_python_comparable(cast(Value, x)) for x in value.data]
    if basic_type == BasicSequenceTypes.SET:
        assert isinstance(value, SequenceType)
        return sorted_no_duplicates(
            to_python_comparable(cast(Value, x)) for x in value.data
        )
    if basic_type == BasicObjectTypes.MAP:
        assert isinstance(value, ObjectType)
        return sorted_no_duplicates(
            (
                (
                    to_python_comparable(cast(Value, pair.key)),
                    to_python_comparable(cast(Value, pair.value)),
                )
                for pair in value.data
            ),
            key=lambda x: x[0],
        )
    if basic_type == BasicNumericTypes.REAL:
        assert isinstance(value, NumberType)
        return ComparableFloat(float(value.data))
    if basic_type == BasicNumericTypes.INTEGER:
        assert isinstance(value, NumberType)
        return value.data
    if basic_type in (
        BasicBooleanTypes.BOOLEAN,
        BasicStringTypes.TEXT,
        BasicNothingTypes.NOTHING,
        BasicStringTypes.ANY,
        BasicStringTypes.UNKNOWN,
    ):
        return value.data

    raise AssertionError(f"Unknown value type: {value}")


@define
class ExceptionValue:
    """An exception that was thrown while executing the user context."""

    message: str
    type: str = ""
    stacktrace: str = ""
    additional_message_keys: list[str] = field(factory=list)

    def readable(self, omit_type) -> str:
        if self.type and not omit_type:
            return f"{self.type}: {self.message}"
        else:
            return self.message
