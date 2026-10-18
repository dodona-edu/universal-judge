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
- Identifiers in all caps are considered "global" variables by default and are read
  from the submission. This can be overridden by adding an explicit scope.

From the Python grammar (at https://docs.python.org/3/library/ast.html#abstract
-grammar),
the following is supported:

- Assignments (without type annotation if it is a simple expression)
- Constant values
- Collection and datastructure literals
- Negation operator
- Function calls
- Keyword arguments (i.e. named arguments)
- Properties (i.e. attributes)

Some identifiers are reserved and cannot be used generally:

- tested
- suite
- submission
"""

import ast
import collections
import io
import tokenize
from decimal import Decimal
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
    ScopeType,
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


# Names that refer to a scope when used as the left-hand side of an attribute,
# e.g. `submission.foo()`. Using them on their own is an error.
_SCOPE_KEYWORDS = {
    "submission": ScopeType.SUBMISSION,
    "suite": ScopeType.SUITE,
    "tested": ScopeType.TESTED,
}


def _scope_keyword(node: ast.expr) -> ScopeType | None:
    """
    Check if the node is a scope keyword, e.g. the `submission` in
    `submission.foo()`, and return the corresponding scope type.
    """
    if isinstance(node, ast.Name):
        return _SCOPE_KEYWORDS.get(node.id)
    return None


def _function_type_for(name: str) -> FunctionType:
    # Consider function calls that start with a capital to be constructors.
    return FunctionType.CONSTRUCTOR if name[0].isupper() else FunctionType.FUNCTION


def _scoped_identifier(name: str, scope_type: ScopeType) -> Identifier:
    """A variable in an explicit scope, e.g. `submission.x` or `suite.x`."""
    if scope_type == ScopeType.TESTED:
        raise InvalidDslError(
            f"The 'tested' scope is reserved for future use, got 'tested.{name}'"
        )
    if scope_type == ScopeType.SUBMISSION and name.isupper():
        # Consistent with the legacy bare `X` form; casing is decided per language.
        name = name.lower()
    return Identifier(name=name, scope=scope_type)


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


def _convert_assignment_target(node: ast.expr) -> Identifier:
    if isinstance(node, ast.Name):
        if _scope_keyword(node) is not None:
            raise InvalidDslError(
                f"'{node.id}' is reserved and cannot be used as a variable name"
            )

        return Identifier(name=node.id, scope=ScopeType.SUITE)
    elif isinstance(node, ast.Attribute):
        name = node.attr
        scope = _scope_keyword(node.value)
        if scope is ScopeType.SUBMISSION:
            raise InvalidDslError(
                f"Cannot assign to a global in the submission scope, "
                f"got 'submission.{name}'"
            )
        elif scope is ScopeType.SUITE:
            return Identifier(name=name, scope=scope)
        elif scope is ScopeType.TESTED:
            raise InvalidDslError(
                f"The 'tested' scope is reserved for future use, got 'tested.{name}'"
            )
        else:
            return Identifier(
                name=name, scope=_convert_expression(node.value, is_return=False)
            )
    else:
        raise InvalidDslError(f"Unsupported assignment target type: {type(node)}")


def _convert_ann_assignment(node: ast.AnnAssign) -> Assignment:
    target = _convert_assignment_target(node.target)
    if target.scope is not ScopeType.SUITE:
        actual = ast.dump(node)
        raise InvalidDslError(
            f"""
            You can only assign to simple variables when using type hints.
            The full assignment is:
            {actual}
            """
        )

    if not node.value:
        actual = ast.dump(node)
        raise InvalidDslError(f"Assignment must have a value, got {actual}")

    value = _convert_expression(node.value, False)
    if isinstance(node.annotation, ast.Name):
        type_ = node.annotation.id
    elif isinstance(node.annotation, ast.Constant) and isinstance(
        node.annotation.value, str
    ):
        type_ = node.annotation.value
    else:
        raise InvalidDslError("Type hints should be simple values.")

    # Check if the type is built-in type or not.
    is_our_type = any(type_ in x.__members__.values() for x in get_args(AllTypes))
    if not is_our_type:
        type_ = VariableType(data=type_)

    return VariableAssignment(
        variable=target,
        expression=value,
        type=cast(VariableType | AllTypes, type_),
    )


def _convert_assignment(node: ast.Assign) -> Assignment:
    if (n := len(node.targets)) != 1:
        raise InvalidDslError(
            f"You must assign to exactly one variable, but got {n} variables."
        )

    target = _convert_assignment_target(node.targets[0])
    value = _convert_expression(node.value, False)

    if target.scope is ScopeType.SUITE:
        # Attempt to determine the type of the variable we are creating in the suite.
        type_ = None
        if isinstance(value, Value):
            type_ = value.type
        elif isinstance(value, FunctionCall) and value.type == FunctionType.CONSTRUCTOR:
            type_ = VariableType(data=value.name)

        if not type_:
            raise InvalidDslError(
                f"Could not deduce the type of variable {target.name}: add a type "
                f"annotation."
            )

        assert isinstance(type_, AllTypes | VariableType)
        return VariableAssignment(variable=target, expression=value, type=type_)

    # The helper guarantees the scope is an expression.
    return PropertyAssignment(target=target, expression=value)


def _convert_call(node: ast.Call) -> FunctionCall:
    if isinstance(node.func, ast.Name):
        if _scope_keyword(node.func) is not None:
            raise InvalidDslError(f"'{node.func.id}' is reserved and cannot be used")
        our_type = _function_type_for(node.func.id)
        name = node.func.id
        scope = ScopeType.SUBMISSION
    elif isinstance(node.func, ast.Attribute):
        name = node.func.attr
        scope = _scope_keyword(node.func.value)
        if scope is ScopeType.SUBMISSION:
            our_type = _function_type_for(name)
        elif scope is ScopeType.SUITE:
            raise InvalidDslError(
                f"A test suite cannot define and thus call functions, got 'suite."
                f"{name}'"
            )
        elif scope is ScopeType.TESTED:
            raise InvalidDslError(
                f"The 'tested' scope is reserved for future use, got 'tested.{name}'"
            )
        else:
            our_type = FunctionType.FUNCTION
            scope = _convert_expression(node.func.value, False)
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

    return FunctionCall(type=our_type, name=name, scope=scope, arguments=arguments)


def _convert_constant(node: ast.Constant) -> Value:
    try:
        return serialize_from_python(node.value)
    except TypeError as e:
        raise InvalidDslError("Unknown constant value", e)


def _convert_expression(node: ast.expr, is_return: bool) -> Expression:
    if _is_type_cast(node):
        assert isinstance(node, ast.Call)
        assert isinstance(node.func, ast.Name)

        # "Casts" of sequence types can also be used a constructor for an empty
        # sequence.
        # For example, "set()", "map()", ...
        nr_of_args = len(node.args)
        if (empty_value := _is_and_get_allowed_empty(node)) and nr_of_args == 0:
            value = empty_value
        else:
            assert isinstance(node.func, ast.Name)
            if nr_of_args != 1:
                if _is_and_get_allowed_empty(node) is not None:
                    error = f"""
                    The cast function '{node.func.id}' must have either zero or one 
                    arguments:
                    - Zero if you want to use it to represent an empty value, 
                    e.g. '{node.func.id}()'
                    - One if you want to cast another value to the type '
{node.func.id}'.
                    """
                    raise InvalidDslError(error)
                else:
                    error = f"""
                    The cast function '{node.func.id}' must have exact one argument, 
                    but found {nr_of_args}.
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
        scope = _scope_keyword(node.value)
        if scope is not None:
            return _scoped_identifier(node.attr, scope)

        return Identifier(
            name=node.attr,
            scope=_convert_expression(node.value, is_return),
        )
    elif isinstance(node, ast.Name):
        # We have special support for "Null" and "Undefined"
        if node.id == "Null":
            return serialize_from_python(None, BasicNothingTypes.NOTHING)
        elif node.id == "Undefined":
            return serialize_from_python(None, AdvancedNothingTypes.UNDEFINED)
        elif _scope_keyword(node) is not None:
            raise InvalidDslError(f"'{node.id}' is reserved and cannot be used")
        elif node.id.isupper():
            return Identifier(name=node.id.lower(), scope=ScopeType.SUBMISSION)
        else:
            return Identifier(name=node.id, scope=ScopeType.SUITE)
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
    comment = ""
    tokens = tokenize.generate_tokens(io.StringIO(code).readline)
    # The "maxlen" is 3 because, the tokenizer will always generate a NEWLINE
    # and ENDMARKER token at the end. So a comment comes just before those 2 tokens.
    candidates = collections.deque(tokens, maxlen=3)
    if len(candidates):
        candidate = candidates.popleft()
        if candidate.type == tokenize.COMMENT:
            comment = candidate.string.lstrip("#").strip()
    return comment


@overload
def parse_string(code: str, is_return: Literal[True]) -> Expression: ...


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
    except InvalidDslError:
        raise
    except Exception as e:
        raise InvalidDslError("Invalid DSL") from e
