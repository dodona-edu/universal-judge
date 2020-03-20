"""
Responsible for the "Abstract Syntax Tree", meaning the expressions and statements.

See the manuscript for a complete overview, but an important point is that the
statements and expressions are intentionally kept simple. We don't support arbitrary
statements and expressions, since we are not implementing a universal language.

In this line of thought, an expression is explicitly not a statement.
"""
from dataclasses import field
from enum import Enum
from typing import Optional, List, Union

from pydantic import root_validator
from pydantic.dataclasses import dataclass

from features import Constructs, FeatureSet, combine_features, WithFeatures
from datatypes import ExtendedTypes
from serialisation import Value


class Identifier(str, WithFeatures):
    """Represents an identifier."""

    def get_used_features(self) -> FeatureSet:
        return FeatureSet(Constructs.NOTHING, set())


class FunctionType(str, Enum):
    FUNCTION = "function"
    """
    A top-level function call. In some languages, this might be translated to a
    NAMESPACE function (e.g. in Java, this is translated to a static method).
    """
    NAMESPACE = "namespace"
    """
    A function in a namespace. The namespace can be an instance, in which case it
    is a method (e.g. Java or Python), but it can also be a function inside a module
    (e.g. Haskell).
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
class FunctionCall(WithFeatures):
    """
    Represents a function call.
    """
    type: FunctionType
    name: str
    namespace: Optional[str] = None
    arguments: List['Expression'] = field(default_factory=list)

    @root_validator
    def namespace_requirements(cls, values):
        type_ = values.get("type")
        namespace_ = values.get("namespace")
        if type_ == FunctionType.NAMESPACE and not namespace_:
            raise ValueError("Namespace functions must have a namespace.")
        if type_ == FunctionType.PROPERTY and not namespace_:
            raise ValueError("Property functions must have a namespace.")
        return values

    def get_used_features(self) -> FeatureSet:
        constructs = Constructs.FUNCTION_CALL

        # Get OOP features.
        if self.type in (FunctionType.PROPERTY, FunctionType.CONSTRUCTOR):
            constructs |= Constructs.OBJECTS

        base_features = FeatureSet(constructs=constructs, types=set())
        argument_features = [x.get_used_features() for x in self.arguments]

        return combine_features([base_features] + argument_features)


Expression = Union[FunctionCall, Identifier, Value]


@dataclass
class VariableType:
    type: ExtendedTypes
    data: Optional[str] = None


@dataclass
class Assignment(WithFeatures):
    """
    Assigns the return value of a function to a variable. Because the expression
    part is pretty simple, the type of the value is determined by looking at the
    expression. It is also possible to define the type. If the type cannot be
    determined and it is not specified, this is an error.
    """
    name: str
    expression: Expression
    type: VariableType

    def replace_expression(self, expression: Expression) -> 'Assignment':
        return Assignment(name=self.name, expression=expression, type=self.type)

    def get_used_features(self) -> FeatureSet:
        base = FeatureSet(Constructs.ASSIGNMENT, set())
        other = self.expression.get_used_features()

        return combine_features([base, other])


Statement = Assignment


# Update the forward references, which fixes the schema generation.
# See https://pydantic-docs.helpmanual.io/usage/postponed_annotations/#self-referencing-models
FunctionCall.__pydantic_model__.update_forward_refs()
