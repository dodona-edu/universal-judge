"""
The serialization format.

This is the authoritative source of the format. For example, json-schema is generated from this
code.

This module does not concern itself with actual decoding or encoding; it is purely concerned about
the format itself and parsing of the format itself (some might call this the meta-concerns).

The encoding and decoding of instances of values in this format are done in the language
implementations itself.

A json-schema can be generated from this format by executing the module on the command line. The
schema will be printed to stdout.
"""
import json
from enum import Enum
from typing import Union, List, Dict

from pydantic import BaseModel
from pydantic.dataclasses import dataclass
from typing_inspect import get_args


class SerialisationError(ValueError):
    def __init__(self, message, additional_errors=None):
        ValueError.__init__(self, message)
        self.additional_errors = additional_errors or []

    def __str__(self):
        original = super().__str__()
        additional_lines = "\n".join(f"* {error}" for error in self.additional_errors)
        return f"{original}\nAdditional errors:\n {additional_lines}"


class NumericTypes(str, Enum):
    INTEGER = "integer"
    REAL = "real"


@dataclass
class NumberType:
    type: NumericTypes
    data: Union[int, float]


class StringTypes(str, Enum):
    TEXT = "text"
    UNKNOWN = "unknown"


@dataclass
class StringType:
    type: StringTypes
    data: str


class BooleanTypes(str, Enum):
    BOOLEAN = "boolean"


@dataclass
class BooleanType:
    type: BooleanTypes
    data: bool


class SequenceTypes(str, Enum):
    LIST = "list"
    SET = "set"


@dataclass
class SequenceType:
    type: SequenceTypes
    data: List['Value']


class ObjectTypes(str, Enum):
    OBJECT = "object"


@dataclass
class ObjectType:
    type: ObjectTypes
    data: Dict[str, 'Value']


# A value is one of the preceding types.
Value = Union[SequenceType, BooleanType, StringType, NumberType, ObjectType]

# Update the forward references, which fixes the schema generation.
# See https://pydantic-docs.helpmanual.io/usage/postponed_annotations/#self-referencing-models
ObjectType.__pydantic_model__.update_forward_refs()
SequenceType.__pydantic_model__.update_forward_refs()


class _SerialisationSchema(BaseModel):
    """The schema for serialising data."""
    __root__: Value


def generate_schema():
    """
    Generate a json schema for the serialisation type. It will be printed on stdout.
    """
    sc = _SerialisationSchema.schema()
    sc['$id'] = "universal-judge/serialisation"
    sc['$schema'] = "http://json-schema.org/schema#"
    print(json.dumps(sc, indent=2))


def parse(value: str) -> Value:
    """
    Parse the json of a value into the relevant data structures.

    If ``value`` is not valid json, a :class:`SerialisationError` will be thrown.

    :param value: The json to be parsed.
    :return: The parsed data.
    """
    try:
        parsed_json = json.loads(value)
    except Exception as e:
        raise SerialisationError(f"Could not parse {value} as valid json.")

    # We try each value until we find one that works, or we throw an error.
    print(parsed_json)
    errors = []
    for clazz in get_args(Value):
        try:
            return clazz(**parsed_json)
        except (TypeError, ValueError) as e:
            errors.append(e)

    raise SerialisationError(f"Could not find valid type for {value}.", additional_errors=errors)


def get_readable_representation(value: Value):
    """
    Get a readable representation of the data. In many cases, this is just the Python type
    that will be returned as a string.
    """
    if value is None:
        return ""
    if isinstance(value, SequenceType):
        values = [get_readable_representation(x) for x in value.data]
        if value.type == SequenceTypes.LIST:
            return str(values)
        elif value.type == SequenceTypes.SET:
            return str(set(values))
        else:
            raise AssertionError("Forgot a type?")
    elif isinstance(value, ObjectType):
        values = {x: get_readable_representation(y) for x, y in value.data}
        return str(values)
    else:
        return str(value.data)


if __name__ == '__main__':
    generate_schema()
