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
import json
import logging
import math
from dataclasses import field, replace
from decimal import Decimal
from enum import Enum
from typing import Union, List, Literal, Optional, Any, Iterable, Tuple, Dict

from pydantic import BaseModel, root_validator, Field
from pydantic.dataclasses import dataclass
from pydantic.fields import Undefined

from tested.dodona import ExtendedMessage, Status
from .datatypes import (NumericTypes, StringTypes, BooleanTypes,
                        SequenceTypes, ObjectTypes, NothingTypes, SimpleTypes,
                        resolve_to_basic, AllTypes, BasicSequenceTypes,
                        BasicObjectTypes, BasicNumericTypes, BasicBooleanTypes,
                        BasicStringTypes, BasicNothingTypes)
from .features import FeatureSet, combine_features, WithFeatures, Construct
from .utils import get_args, flatten

logger = logging.getLogger(__name__)

WrappedAllTypes = Union[
    AllTypes, Tuple[SequenceTypes, 'WrappedAllTypes'],
    Tuple[ObjectTypes, Tuple['WrappedAllTypes', 'WrappedAllTypes']]
]


def _get_type_for(expression: 'Expression') -> WrappedAllTypes:
    if isinstance(expression, SequenceType):
        return expression.type, expression.get_content_type()
    elif isinstance(expression, ObjectType):
        return expression.type, (
            expression.get_key_type(), expression.get_value_type()
        )
    elif isinstance(expression, get_args(Value)):
        return expression.type
    else:
        return BasicStringTypes.ANY


def _get_combined_types(types: Iterable[WrappedAllTypes]) -> WrappedAllTypes:
    type_dict = dict()
    for data_type in types:
        if isinstance(data_type, tuple):
            if isinstance(data_type[1], tuple):
                try:
                    type_dict[data_type[0]][0].add(data_type[1][0])
                    type_dict[data_type[0]][1].add(data_type[1][1])
                except KeyError:
                    type_dict[data_type[0]] = (set(data_type[1][0]),
                                               set(data_type[1][1]))
            else:
                try:
                    type_dict[data_type[0]].add(data_type[1])
                except KeyError:
                    type_dict[data_type[0]] = {data_type[1]}
        else:
            type_dict[data_type] = None
    if len(type_dict) == 1:
        # noinspection PyTypeChecker
        key_type, sub_type = next(iter(type_dict.items()))
        if sub_type is None:
            return key_type
        elif isinstance(sub_type, tuple):
            return key_type, (_get_combined_types(sub_type[0]),
                              _get_combined_types(sub_type[1]))
        else:
            return key_type, _get_combined_types(sub_type)
    else:
        return BasicStringTypes.ANY


class WithFunctions:
    def get_functions(self) -> Iterable['FunctionCall']:
        raise NotImplementedError


@dataclass
class NumberType(WithFeatures, WithFunctions):
    type: NumericTypes
    data: Union[Decimal, int, float]

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), {self.type})

    def get_functions(self) -> Iterable['FunctionCall']:
        return []

    # noinspection PyMethodParameters
    @root_validator
    def check_passwords_match(cls, values):
        if resolve_to_basic(values.get("type")) == BasicNumericTypes.INTEGER:
            values["data"] = values["data"].to_integral_value()

        return values


@dataclass
class StringType(WithFeatures, WithFunctions):
    type: StringTypes
    data: str

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), {self.type})

    def get_functions(self) -> Iterable['FunctionCall']:
        return []


@dataclass
class BooleanType(WithFeatures, WithFunctions):
    type: BooleanTypes
    data: bool

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), {self.type})

    def get_functions(self) -> Iterable['FunctionCall']:
        return []


@dataclass
class SequenceType(WithFeatures, WithFunctions):
    type: SequenceTypes
    data: List['Expression']

    def get_used_features(self) -> FeatureSet:
        base_features = FeatureSet(set(), {self.type})
        nested_features = [x.get_used_features() for x in self.data]
        combined = combine_features([base_features] + nested_features)
        content_type = self.get_content_type()
        if content_type == BasicStringTypes.ANY:
            combined = combine_features([FeatureSet(
                {Construct.HETEROGENEOUS_COLLECTIONS}, set()
            )])
        return combined

    def get_content_type(self) -> WrappedAllTypes:
        """
        Attempt to get a type for the content of the container. The function will
        attempt to get the most specific type possible.
        """
        return _get_combined_types(_get_type_for(element) for element in self.data)

    def get_functions(self) -> Iterable['FunctionCall']:
        return flatten(x.get_functions() for x in self.data)


@dataclass
class ObjectKeyValuePair(WithFeatures, WithFunctions):
    key: 'Expression'
    value: 'Expression'

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
        nested_features = [self.key.get_used_features(),
                           self.value.get_used_features()]
        return combine_features(nested_features)

    def get_functions(self) -> Iterable['FunctionCall']:
        return self.value.get_functions()


@dataclass
class ObjectType(WithFeatures, WithFunctions):
    type: ObjectTypes
    data: List[ObjectKeyValuePair]

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
        return _get_combined_types(
            element.get_value_type() for element in self.data)

    def get_used_features(self) -> FeatureSet:
        base_features = FeatureSet(set(), {self.type})
        nested_features = [x.get_used_features() for x in self.data]
        return combine_features([base_features] + nested_features)

    def get_functions(self) -> Iterable['FunctionCall']:
        return flatten(flatten(
            (pair.key.get_functions(), pair.value.get_functions())
        ) for pair in self.data)


@dataclass
class NothingType(WithFeatures, WithFunctions):
    type: NothingTypes = BasicNothingTypes.NOTHING
    data: Literal[None] = None

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), {self.type})

    def get_functions(self) -> Iterable['FunctionCall']:
        return []


# A value is one of the preceding types.
Value = Union[
    NumberType, StringType, BooleanType, SequenceType, ObjectType, NothingType
]


class Identifier(str, WithFeatures, WithFunctions):
    """Represents an identifier."""

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(set(), set())

    def get_functions(self) -> Iterable['FunctionCall']:
        return []

    @classmethod
    def __get_validators__(cls):
        # one or more validators may be yielded which will be called in the
        # order to validate the input, each validator will receive as an input
        # the value returned from the previous validator
        yield cls.validate

    @classmethod
    def validate(cls, v):
        if not isinstance(v, str):
            raise TypeError('string required')
        return Identifier(v)


class FunctionType(str, Enum):
    FUNCTION = "function"
    """
    A function expression. A function can be in a namespace if it is provided.
    The namespace can be an instance, in which case it is a method (e.g. Java or
    Python), but it can also be a function inside a module (e.g. Haskell).
    E.g. with Java: a function without a given namespace will have the namespace of
    it's implementing class
    """
    CONSTRUCTOR = "constructor"
    """
    A constructor.
    """
    PROPERTY = "property"
    """
    Access a property on an object.
    """


@dataclass
class NamedArgument(WithFeatures, WithFunctions):
    """Represents a named argument for a function."""
    name: str
    value: 'Expression'

    def get_used_features(self) -> FeatureSet:
        this = FeatureSet(constructs={Construct.NAMED_ARGUMENTS}, types=set())
        expression = self.value.get_used_features()
        return combine_features([this, expression])

    def get_functions(self) -> Iterable['FunctionCall']:
        return self.value.get_functions()


@dataclass
class FunctionCall(WithFeatures, WithFunctions):
    """
    Represents a function expression.
    """
    type: FunctionType
    name: str
    namespace: Optional[str] = None
    arguments: List[Union['Expression', NamedArgument]] \
        = field(default_factory=list)

    # noinspection PyMethodParameters
    @root_validator
    def namespace_requirements(cls, values):
        type_ = values.get("type")
        namespace_ = values.get("namespace")
        if type_ == FunctionType.PROPERTY and not namespace_:
            raise ValueError("Property functions must have a namespace.")
        return values

    # noinspection PyMethodParameters
    @root_validator
    def properties_have_no_args(cls, values):
        type_ = values.get("type")
        args = values.get("args")
        if type_ == FunctionType.PROPERTY and args:
            raise ValueError("You cannot have arguments for a property!")
        return values

    def get_used_features(self) -> FeatureSet:
        constructs = {Construct.FUNCTION_CALLS}

        # Get OOP features.
        if self.type in (FunctionType.PROPERTY, FunctionType.CONSTRUCTOR):
            constructs.add(Construct.OBJECTS)

        base_features = FeatureSet(constructs=constructs, types=set())
        argument_features = [x.get_used_features() for x in self.arguments]

        return combine_features([base_features] + argument_features)

    def get_functions(self) -> Iterable['FunctionCall']:
        return [self, *[list(x.get_functions()) for x in self.arguments]]


@dataclass
class VariableType:
    data: str
    type: Literal['custom'] = 'custom'


Expression = Union[Identifier, FunctionCall, Value]


@dataclass
class Assignment(WithFeatures, WithFunctions):
    """
    Assigns the return value of a function to a variable. Because the expression
    part is pretty simple, the type of the value is determined by looking at the
    expression. It is also possible to define the type. If the type cannot be
    determined and it is not specified, this is an error.
    """
    variable: str
    expression: Expression
    type: Union[AllTypes, VariableType]

    def replace_expression(self, expression: Expression) -> 'Assignment':
        return Assignment(variable=self.variable, expression=expression,
                          type=self.type)

    def get_used_features(self) -> FeatureSet:
        base = FeatureSet({Construct.ASSIGNMENTS}, set())
        other = self.expression.get_used_features()

        return combine_features([base, other])

    def get_functions(self) -> Iterable['FunctionCall']:
        return self.expression.get_functions()


Statement = Union[Assignment, Expression]

# Update the forward references, which fixes the schema generation.
ObjectType.__pydantic_model__.update_forward_refs()
SequenceType.__pydantic_model__.update_forward_refs()
NamedArgument.__pydantic_model__.update_forward_refs()
FunctionCall.__pydantic_model__.update_forward_refs()
ObjectKeyValuePair.__pydantic_model__.update_forward_refs()


def as_basic_type(value: Value) -> Value:
    """Convert a value's type to a basic type."""
    new_type = resolve_to_basic(value.type)
    # noinspection PyDataclass
    return replace(value, type=new_type)


class _SerialisationSchema(BaseModel):
    """The schema for serialising data."""
    __root__: Value


def generate_schema():
    """
    Generate a json schema for the serialisation type. It will be printed on stdout.
    """
    sc = _SerialisationSchema.schema()
    sc['$id'] = "tested/serialisation"
    sc['$schema'] = "http://json-schema.org/schema#"
    print(json.dumps(sc, indent=2))


def parse_value(value: str) -> Value:
    """
    Parse the json of a value into the relevant data structures.

    If ``value`` is not valid json, a :class:`SerialisationError` will be thrown.

    :param value: The json to be parsed.
    :return: The parsed data.
    """
    try:
        parsed_json = json.loads(value)
    except Exception as e:
        raise ValueError(f"Could not parse {value} as valid json.", e)

    # We try each value until we find one that works, or we throw an error.
    errors = []
    for clazz in get_args(Value):
        try:
            return clazz(**parsed_json)
        except (TypeError, ValueError) as e:
            errors.append(e)

    logger.debug(f"Could not parse value, errors are {errors}. Could be normal!")

    raise TypeError(
        f"Could not find valid type for {value}."
    )


class PrintingDecimal:

    def __init__(self, decimal: Decimal):
        self.decimal = decimal

    def __repr__(self):
        return str(self.decimal)


def _convert_to_python(value: Optional[Value], for_printing=False) -> Any:
    """
    Convert the parsed values into the proper Python type. This is basically
    the same as de-serialising a value, but this function is currently not re-used
    in the Python implementation, since run-time de-serialisation is not supported.
    :param value: The parsed value.
    :param for_printing: If the result will be used for printing or not.
    :return: The Python value.
    """
    if value is None:
        return None

    # If we have a type for which the data is usable in Python, use it.
    if isinstance(value.type, get_args(SimpleTypes)):
        # If we have floats or ints, convert them to Python.
        if value.type in (NumericTypes.SINGLE_PRECISION,
                          NumericTypes.DOUBLE_PRECISION):
            return float(str(value.data))
        if value.type != NumericTypes.FIXED_PRECISION:
            return int(str(value.data))
        if for_printing:
            return PrintingDecimal(value.data)

        return value.data

    if isinstance(value.type, get_args(SequenceTypes)):
        values = [_convert_to_python(x) for x in value.data]
        basic_type = resolve_to_basic(value.type)
        if basic_type == SequenceTypes.SEQUENCE:
            return values
        if basic_type == SequenceTypes.SET:
            return set(values)
        raise AssertionError(f"Unknown basic sequence type {basic_type}.")

    if isinstance(value.type, get_args(ObjectTypes)):
        values = {x: _convert_to_python(y) for x, y in value.data.items()}
        return values

    if isinstance(value, NothingType):
        return None

    # Unknown type.
    logger.warning(f"Unknown data type {value.type} will be interpreted as string.")
    return str(value.data)


class ComparableFloat:

    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        # noinspection PyBroadException
        try:
            return math.isclose(self.value, other.value)
        except Exception:
            return False

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return repr(self.value)

    def __bool__(self):
        return bool(self.value)


def to_python_comparable(value: Optional[Value]):
    """
    Convert the value into a comparable Python value. Most values are just
    converted to their
    builtin Python variant. Some, however, are not: floats are converted into a
    wrapper class, that
    allows comparison.

    Note that this means that the types in the return value can be different from
    what is channel;
    the returning types are only guaranteed to support eq, str, repr and bool.
    """
    basic_type = resolve_to_basic(value.type)
    if value is None:
        return None
    if basic_type == BasicSequenceTypes.SEQUENCE:
        return [to_python_comparable(x) for x in value.data]
    if basic_type == BasicSequenceTypes.SET:
        return {to_python_comparable(x) for x in value.data}
    if basic_type == BasicObjectTypes.MAP:
        return [(to_python_comparable(pair.key), to_python_comparable(pair.value))
                for pair in value.data]
    if basic_type == BasicNumericTypes.RATIONAL:
        return ComparableFloat(float(value.data))
    if basic_type == BasicNumericTypes.INTEGER:
        return value.data
    if basic_type in (BasicBooleanTypes.BOOLEAN, BasicStringTypes.TEXT,
                      BasicNothingTypes.NOTHING, BasicStringTypes.ANY):
        return value.data

    raise AssertionError(f"Unknown value type: {value}")


class EvalResult(BaseModel):
    result: Union[bool, Status]
    readable_expected: Optional[str] = None
    readable_actual: Optional[str] = None
    messages: List[ExtendedMessage] = Field(Undefined, default_factory=list)


@dataclass
class ExceptionValue(WithFeatures):
    """An exception that was thrown while executing the user context."""
    message: str
    stacktrace: str = ""

    def get_used_features(self) -> FeatureSet:
        return FeatureSet({Construct.EXCEPTIONS}, types=set())

    def readable(self) -> str:
        return f"{self.message}".strip()


if __name__ == '__main__':
    generate_schema()
