import json
from typing import List

from tested.datatypes import (
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.languages.preparation import (
    PreparedContext,
    PreparedExecutionUnit,
    PreparedFunctionCall,
    PreparedTestcase,
    PreparedTestcaseStatement,
)
from tested.languages.utils import convert_unknown_type
from tested.serialisation import (
    Assignment,
    Expression,
    FunctionCall,
    FunctionType,
    Identifier,
    NamedArgument,
    ObjectType,
    SequenceType,
    SpecialNumbers,
    Statement,
    StringType,
    Value,
    as_basic_type,
)
from tested.testsuite import MainInput


def convert_arguments(
    arguments: List[Expression | NamedArgument], with_namespace=False
) -> str:
    results = []
    for arg in arguments:
        if isinstance(arg, NamedArgument):
            results.append(f"{arg.name}={convert_statement(arg.value, with_namespace)}")
        else:
            results.append(convert_statement(arg, with_namespace))
    return ", ".join(results)


def convert_value(value: Value) -> str:
    # Handle some advanced types.
    if value.type == AdvancedSequenceTypes.TUPLE:
        assert isinstance(value, SequenceType)
        return f"({convert_arguments(value.data)})"
    elif value.type in (
        AdvancedNumericTypes.DOUBLE_EXTENDED,
        AdvancedNumericTypes.FIXED_PRECISION,
    ):
        if not isinstance(value.data, SpecialNumbers):
            return f'Decimal("{value.data}")'
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return f'Decimal(float("nan"))'
        elif value.data == SpecialNumbers.POS_INFINITY:
            return f'Decimal(float("inf"))'
        else:
            assert value.data == SpecialNumbers.NEG_INFINITY
            return f'Decimal(float("-inf"))'
    # Handle basic types
    value = as_basic_type(value)
    if value.type in (BasicNumericTypes.INTEGER, BasicNumericTypes.REAL):
        if not isinstance(value.data, SpecialNumbers):
            return str(value.data)
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return "float('nan')"
        elif value.data == SpecialNumbers.POS_INFINITY:
            return "float('inf')"
        else:
            assert SpecialNumbers.NEG_INFINITY
            return "float('-inf')"
    elif value.type == BasicStringTypes.TEXT:
        return repr(value.data)
    elif value.type == BasicBooleanTypes.BOOLEAN:
        return str(value.data)
    elif value.type == BasicNothingTypes.NOTHING:
        return "None"
    elif value.type == BasicSequenceTypes.SEQUENCE:
        assert isinstance(value, SequenceType)
        return f"[{convert_arguments(value.data)}]"
    elif value.type == BasicSequenceTypes.SET:
        assert isinstance(value, SequenceType)
        return f"{{{convert_arguments(value.data)}}}"
    elif value.type == BasicObjectTypes.MAP:
        assert isinstance(value, ObjectType)
        result = "{"
        for i, pair in enumerate(value.data):
            result += convert_statement(pair.key, True)
            result += ": "
            result += convert_statement(pair.value, True)
            if i != len(value.data) - 1:
                result += ", "
        result += "}"
        return result
    elif value.type == BasicStringTypes.UNKNOWN:
        assert isinstance(value, StringType)
        return convert_unknown_type(value)
    raise AssertionError(f"Invalid literal: {value!r}")


def convert_function_call(function: FunctionCall, with_namespace=False) -> str:
    result = ""
    if function.namespace and (
        not (isinstance(function, PreparedFunctionCall) and function.has_root_namespace)
        or with_namespace
    ):
        result += convert_statement(function.namespace, with_namespace) + "."
    result += function.name
    if function.type != FunctionType.PROPERTY:
        result += f"({convert_arguments(function.arguments, with_namespace)})"
    return result


def convert_statement(statement: Statement, with_namespace=False) -> str:
    if isinstance(statement, Identifier):
        return statement
    elif isinstance(statement, FunctionCall):
        return convert_function_call(statement, with_namespace)
    elif isinstance(statement, Value):
        return convert_value(statement)
    elif isinstance(statement, Assignment):
        return (
            f"{statement.variable} = "
            f"{convert_statement(statement.expression, with_namespace)}"
        )
    raise AssertionError(f"Unknown statement: {statement!r}")


def convert_execution_unit(pu: PreparedExecutionUnit) -> str:
    result = """
import values
import sys
import importlib
from decimal import Decimal
"""

    # Import the language specific evaluators we will need.
    for name in pu.evaluator_names:
        result += f"import {name}\n"

    # We now open files for results and define some functions.
    result += f"""
value_file = open("{pu.value_file}", "w")
exception_file = open("{pu.exception_file}", "w")

def write_separator():
    value_file.write("--{pu.testcase_separator_secret}-- SEP")
    exception_file.write("--{pu.testcase_separator_secret}-- SEP")
    sys.stderr.write("--{pu.testcase_separator_secret}-- SEP")
    sys.stdout.write("--{pu.testcase_separator_secret}-- SEP")
    sys.stdout.flush()
    sys.stderr.flush()
    value_file.flush()
    exception_file.flush()

def write_context_separator():
    value_file.write("--{pu.context_separator_secret}-- SEP")
    exception_file.write("--{pu.context_separator_secret}-- SEP")
    sys.stderr.write("--{pu.context_separator_secret}-- SEP")
    sys.stdout.write("--{pu.context_separator_secret}-- SEP")
    sys.stdout.flush()
    sys.stderr.flush()
    value_file.flush()
    exception_file.flush()

def send_value(value):
    values.send_value(value_file, value)

def send_exception(exception):
    values.send_exception(exception_file, exception)

def send_specific_value(value):
    values.send_evaluated(value_file, value)

def send_specific_exception(exception):
    values.send_evaluated(exception_file, exception)
"""

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        indent = " " * 4
        result += f"def {pu.execution_name}_context_{i}():\n"
        result += indent + ctx.before + "\n"

        if not ctx.context.has_main_testcase():
            result += indent + f"import {pu.submission_name}\n"
            if i != 0:
                result += (
                    f'{indent}importlib.reload(sys.modules["{pu.submission_name}"])\n'
                )

        tc: PreparedTestcase
        for tc in ctx.testcases:
            result += indent + "write_separator()\n"

            # Prepare command arguments if needed.
            if tc.testcase.is_main_testcase():
                assert isinstance(tc.input, MainInput)
                result += indent + "new_args = [sys.argv[0]]\n"
                wrapped = [json.dumps(a) for a in tc.input.arguments]
                result += f"{indent}new_args.extend([{', '.join(wrapped)}])\n"
                result += indent + "sys.argv = new_args\n"

            result += indent + "try:\n"
            if tc.testcase.is_main_testcase():
                assert isinstance(tc.input, MainInput)
                result += f"{indent*2}import {pu.submission_name}\n"
                if i != 0:
                    result += f'{indent*2}importlib.reload(sys.modules["{pu.submission_name}"])\n'
            else:
                assert isinstance(tc.input, PreparedTestcaseStatement)
                result += (
                    indent * 2
                    + convert_statement(tc.input.input_statement(), True)
                    + "\n"
                )

            result += indent + "except Exception as e:\n"
            result += indent * 2 + convert_statement(tc.exception_statement("e")) + "\n"
            result += indent + "else:\n"
            result += indent * 2 + convert_statement(tc.exception_statement()) + "\n"

        result += indent + ctx.after + "\n"

    for i, ctx in enumerate(pu.contexts):
        result += "write_context_separator()\n"
        result += f"{pu.execution_name}_context_{i}()\n"

    result += """
value_file.close()
exception_file.close()
"""

    return result


def convert_check_function(name: str, function: FunctionCall) -> str:
    return f"""
import {name}
import values
import sys

result = {convert_function_call(function)}
values.send_evaluated(sys.stdout, result)
"""


def convert_encoder(values: List[Value]) -> str:
    result = """
import sys
import values
from decimal import Decimal
"""

    for value in values:
        result += f"values.send_value(sys.stdout, {convert_value(value)})\n"
        result += "print()\n"
    return result
