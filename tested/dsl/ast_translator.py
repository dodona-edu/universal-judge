"""
Translate the Python AST into the TESTed AST.

This module will parse a string with a Python statement and translate it into
the TESTed AST from the serialisation module.

Note that we only support the subset of the Python syntax that is also supported
by our AST.

Some useful notes:

- The parser will convert "casts" in the Python code to one of our types.
- Two special values exist: Null and Undefined.
- Function calls whose name begins with a capital are considered constructors.
- Identifiers in call caps are considered "global" variables.

From the Python grammar (at https://docs.python.org/3/library/ast.html#abstract-grammar),
the following is supported:

- Assignments (without type annotation if it is a simple expression)
- Constant values
- Collection and datastructure literals
- Negation operator
- Function calls
- Keyword arguments (i.e. named arguments)
- Properties (i.e. attributes)
"""

import ast
import tokenize
from decimal import Decimal
from io import BytesIO
from typing import Literal, cast, overload

from attrs import evolve

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedSequenceTypes,
    AllTypes,
    BasicNothingTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
)
from tested.parsing import get_converter
from tested.serialisation import (
    Assignment,
    Expression,
    FunctionCall,
    FunctionType,
    Identifier,
    NamedArgument,
    NumberType,
    ObjectKeyValuePair,
    ObjectType,
    PropertyAssignment,
    SequenceType,
    Statement,
    Value,
    VariableAssignment,
    VariableType,
    serialize_from_python,
)
from tested.utils import get_args


class InvalidDslError(Exception):
    pass


def _is_and_get_allowed_empty(node: ast.Call) -> Value | None:
    """
    Check if we allow this cast without params to represent an "empty" value.
    Returns the empty value if allowed, otherwise None.
    """
    assert isinstance(node.func, ast.Name)
    type_ = get_converter().structure(node.func.id, AllTypes)  # pyright: ignore
    if isinstance(type_, AdvancedSequenceTypes):
        return SequenceType(type=cast(AdvancedSequenceTypes, type_), data=[])
    elif isinstance(type_, BasicSequenceTypes):
        return SequenceType(type=cast(BasicSequenceTypes, type_), data=[])
    elif isinstance(type_, BasicObjectTypes):
        return ObjectType(type=type_, data=[])
    else:
        return None


def _is_type_cast(node: ast.expr) -> bool:
    """
    Check if this is a cast to a specific type or not.
    """
    if not isinstance(node, ast.Call):
        return False
    if not isinstance(node.func, ast.Name):
        return False

    return any(node.func.id in x.__members__.values() for x in get_args(AllTypes))


def _convert_ann_assignment(node: ast.AnnAssign) -> Assignment:
    if not isinstance(node.target, ast.Name):
        actual = ast.dump(node)
        raise InvalidDslError(
            f"""
            You can only assign to simple variables when using type hints.
            You are trying to assign to a target of type {type(node.target)}.
            The full assignment is:
            {actual}
            """
        )
    assert node.value
    value = _convert_expression(node.value, False)
    if isinstance(node.annotation, ast.Name):
        type_ = node.annotation.id
    elif isinstance(node.annotation, ast.Str):
        type_ = node.annotation.s
    else:
        raise InvalidDslError("Type hints should be simple values.")

    # Check if the type is built-in type or not.
    is_our_type = any(type_ in x.__members__.values() for x in get_args(AllTypes))
    if not is_our_type:
        type_ = VariableType(data=type_)

    return VariableAssignment(
        variable=node.target.id,
        expression=value,
        type=cast(VariableType | AllTypes, type_),
    )


def _convert_assignment(node: ast.Assign) -> Assignment:
    if n := len(node.targets) != 1:
        raise InvalidDslError(
            f"You must assign to exactly one variable, but got {n} variables."
        )
    variable = node.targets[0]
    value = _convert_expression(node.value, False)

    if isinstance(variable, ast.Name):
        # Support a few obvious ones, such as constructor calls or literal values.
        type_ = None
        if isinstance(value, Value):
            type_ = value.type
        elif isinstance(value, FunctionCall) and value.type == FunctionType.CONSTRUCTOR:
            type_ = VariableType(data=value.name)

        if not type_:
            raise InvalidDslError(
                f"Could not deduce the type of variable {variable.id}: add a type annotation."
            )

        assert isinstance(type_, AllTypes | VariableType)
        return VariableAssignment(variable=variable.id, expression=value, type=type_)
    elif isinstance(variable, ast.Attribute):
        property_access = _convert_expression(variable, False)
        assert isinstance(property_access, FunctionCall)
        return PropertyAssignment(property=property_access, expression=value)
    else:
        actual = ast.dump(node)
        raise InvalidDslError(
            f"You can only assign to simple variables or attributes, got: {actual}."
        )


def _convert_call(node: ast.Call) -> FunctionCall:
    # We consider function calls that start with a capital to be constructors.
    if isinstance(node.func, ast.Name):
        if node.func.id[0].isupper():
            our_type = FunctionType.CONSTRUCTOR
        else:
            our_type = FunctionType.FUNCTION
        name = node.func.id
        namespace = None
    elif isinstance(node.func, ast.Attribute):
        our_type = FunctionType.FUNCTION
        name = node.func.attr
        namespace = _convert_expression(node.func.value, False)
    else:
        raise InvalidDslError(f"Unknown function type: {type(node.func)}")

    arguments = []

    for arg in node.args:
        arguments.append(_convert_expression(arg, False))

    for keyword in node.keywords:
        arguments.append(
            NamedArgument(
                name=cast(str, keyword.arg),
                value=_convert_expression(keyword.value, False),
            )
        )

    if our_type == FunctionType.PROPERTY and len(arguments) != 0:
        raise InvalidDslError("Passing arguments to property calls is not supported.")

    return FunctionCall(
        type=our_type, name=name, namespace=namespace, arguments=arguments
    )


def _convert_constant(node: ast.Constant) -> Value:
    try:
        return serialize_from_python(node.value)
    except TypeError as e:
        raise InvalidDslError("Unknown constant value", e)


def _convert_expression(node: ast.expr, is_return: bool) -> Expression:
    if _is_type_cast(node):
        assert isinstance(node, ast.Call)
        assert isinstance(node.func, ast.Name)

        # "Casts" of sequence types can also be used a constructor for an empty sequence.
        # For example, "set()", "map()", ...
        nr_of_args = len(node.args)
        if (empty_value := _is_and_get_allowed_empty(node)) and nr_of_args == 0:
            value = empty_value
        else:
            assert isinstance(node.func, ast.Name)
            if nr_of_args != 1:
                if _is_and_get_allowed_empty(node) is not None:
                    error = f"""
                    The cast function '{node.func.id}' must have either zero or one arguments:
                    - Zero if you want to use it to represent an empty value, e.g. '{node.func.id}()'
                    - One if you want to cast another value to the type '{node.func.id}'.
                    """
                    raise InvalidDslError(error)
                else:
                    error = f"""
                    The cast function '{node.func.id}' must have exact one argument, but found {nr_of_args}.
                    For example, '{node.func.id}(...)', where '...' is the value.
                    """
                    raise InvalidDslError(error)
            # We have a cast, so extract the value, but modify the type later on.
            subexpression = node.args[0]
            value = _convert_expression(subexpression, is_return)

            if not isinstance(value, Value):
                raise InvalidDslError(
                    "The argument of a cast function must resolve to a value."
                )
        return evolve(
            value,
            type=get_converter().structure(node.func.id, AllTypes),  # pyright: ignore
        )
    elif isinstance(node, ast.Call):
        if is_return:
            raise InvalidDslError(
                "You cannot use a function call in the expected return value."
            )
        return _convert_call(node)
    elif isinstance(node, ast.Attribute):
        if is_return:
            raise InvalidDslError(
                "You cannot use an attribute in the expected return value."
            )
        return FunctionCall(
            type=FunctionType.PROPERTY,
            name=node.attr,
            namespace=_convert_expression(node.value, is_return),
            arguments=[],
        )
    elif isinstance(node, ast.Name):
        # We have special support for "Null" and "Undefined"
        if node.id == "Null":
            return serialize_from_python(None, BasicNothingTypes.NOTHING)
        elif node.id == "Undefined":
            return serialize_from_python(None, AdvancedNothingTypes.UNDEFINED)
        elif node.id.isupper():
            return FunctionCall(type=FunctionType.PROPERTY, name=node.id.lower())
        else:
            return Identifier(node.id)
    elif isinstance(node, ast.List):
        elements = [_convert_expression(e, is_return) for e in node.elts]
        return SequenceType(type=BasicSequenceTypes.SEQUENCE, data=elements)
    elif isinstance(node, ast.Tuple):
        elements = [_convert_expression(e, is_return) for e in node.elts]
        return SequenceType(type=AdvancedSequenceTypes.TUPLE, data=elements)
    elif isinstance(node, ast.Set):
        elements = [_convert_expression(e, is_return) for e in node.elts]
        return SequenceType(type=BasicSequenceTypes.SET, data=elements)
    elif isinstance(node, ast.Dict):
        elements = [
            ObjectKeyValuePair(
                key=_convert_expression(cast(ast.expr, k), is_return),
                value=_convert_expression(v, is_return),
            )
            for k, v in zip(node.keys, node.values)
        ]
        return ObjectType(type=BasicObjectTypes.MAP, data=elements)
    elif isinstance(node, ast.Constant):
        return _convert_constant(node)
    elif isinstance(node, ast.UnaryOp):
        if not isinstance(node.op, ast.USub):
            raise InvalidDslError(f"Unsupported unary operator {node.op}")
        if not isinstance(node.operand, ast.Constant):
            raise InvalidDslError("Only '-' is supported on literal numbers")
        value = _convert_constant(node.operand)
        if not isinstance(value, NumberType):
            raise InvalidDslError("'-' is only supported on literal numbers")
        assert isinstance(value.data, Decimal | int | float)
        return NumberType(type=value.type, data=-value.data)
    else:
        raise InvalidDslError(f"Unsupported expression type: {type(node)}")


def _convert_statement(node: ast.stmt) -> Statement:
    """
    Convert a Python statement into a TESTed statement.
    """
    if isinstance(node, ast.Assign):
        return _convert_assignment(node)
    elif isinstance(node, ast.AnnAssign):
        return _convert_ann_assignment(node)
    elif isinstance(node, ast.Expr):
        return _convert_expression(node.value, False)
    else:
        raise InvalidDslError(f"Unsupported statement: {type(node)}")


def _translate_to_ast(node: ast.Interactive, is_return: bool) -> Statement:
    if len(node.body) != 1:
        raise InvalidDslError(
            f"Requires exactly one stmt or value, got {len(node.body)}"
        )

    statement_or_expression: ast.stmt = node.body[0]

    if is_return:
        assert isinstance(statement_or_expression, ast.Expr)
        return _convert_expression(statement_or_expression.value, is_return)
    else:
        return _convert_statement(statement_or_expression)


def extract_comment(code: str) -> str:
    """
    Extract the comment from the code.

    :param code: The code to extract the comment from.
    :return: The comment if it exists, otherwise an empty string.
    """
    tokens = tokenize.tokenize(BytesIO(code.encode("utf-8")).readline)
    comments = list(
        map(lambda t: t.string, filter(lambda t: t.type == tokenize.COMMENT, tokens))
    )
    if len(comments) == 0:
        return ""
    comment = comments[0][1:]
    assert isinstance(comment, str)
    return comment.strip()


@overload
def parse_string(code: str, is_return: Literal[True]) -> Value: ...


@overload
def parse_string(code: str, is_return: Literal[False] = False) -> Statement: ...


def parse_string(code: str, is_return=False) -> Statement:
    """
    Parse a string with Python code into our AST.

    :param code: The code to parse.
    :param is_return: If the code must be a value, or if statements are allowed or not.
    :return: The parsed statement.
    """
    try:
        tree = ast.parse(code, mode="single")
        return _translate_to_ast(tree, is_return)
    except Exception as e:
        raise InvalidDslError("Invalid DSL") from e
