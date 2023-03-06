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
- Keyword arguments (ie. named arguments)
- Properties (ie. attributes)
"""

from pydantic import ValidationError

import ast
import dataclasses
from tested.datatypes import (
    AdvancedNothingTypes,
    BasicNothingTypes,
    AllTypes,
    BasicSequenceTypes,
    AdvancedSequenceTypes,
    BasicObjectTypes,
)
from tested.serialisation import (
    Statement,
    Assignment,
    Expression,
    FunctionCall,
    FunctionType,
    Value,
    NamedArgument,
    serialize_from_python,
    NumberType,
    SequenceType,
    ObjectType,
    ObjectKeyValuePair,
    VariableType,
    Identifier,
)
from tested.utils import get_args


class InvalidDslError(Exception):
    pass


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
        raise InvalidDslError("You can only assign to simple variables")
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

    return Assignment(variable=node.target.id, expression=value, type=type_)


def _convert_assignment(node: ast.Assign) -> Assignment:
    # raise InvalidDslError("You need to annotate the variable with a type.")
    if n := len(node.targets) != 1:
        raise InvalidDslError(
            f"You must assign to exactly one variable, but got {n} variables."
        )
    variable = node.targets[0]
    if not isinstance(variable, ast.Name):
        raise InvalidDslError("You can only assign to simple variables")
    value = _convert_expression(node.value, False)

    # Support a few obvious ones, such as constructor calls or literal values.
    type_ = None
    if isinstance(value, get_args(Value)):
        type_ = value.type
    elif isinstance(value, FunctionCall) and value.type == FunctionType.CONSTRUCTOR:
        type_ = VariableType(data=value.name)

    if not type_:
        raise InvalidDslError(
            "Could deduce the type of the variable for assignment: add a type annotation."
        )

    return Assignment(variable=variable.id, expression=value, type=type_)


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
                name=keyword.arg, value=_convert_expression(keyword.value, False)
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
        if n := len(node.args) != 1:
            raise InvalidDslError(
                f"A cast function must have exactly one argument, found {n}"
            )
        # We have a cast, so extract the value, but modify the type later on.
        subexpression = node.args[0]
        value = _convert_expression(subexpression, is_return)

        if not isinstance(value, get_args(Value)):
            raise InvalidDslError(
                "The argument of a cast function must resolve to a value."
            )

        assert isinstance(node.func, ast.Name)
        return dataclasses.replace(value, type=node.func.id)
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
                _convert_expression(k, is_return), _convert_expression(v, is_return)
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
    except ValidationError as e:
        raise InvalidDslError("Probably type error in DSL") from e
    except SyntaxError as e:
        raise InvalidDslError("Invalid syntax in DSL") from e
