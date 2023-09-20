import shlex
from collections.abc import Callable

from tested.datatypes import AdvancedStringTypes, BasicStringTypes
from tested.languages.preparation import (
    PreparedContext,
    PreparedExecutionUnit,
    PreparedTestcase,
    PreparedTestcaseStatement,
)
from tested.languages.utils import convert_unknown_type
from tested.serialisation import (
    Assignment,
    FunctionCall,
    FunctionType,
    Identifier,
    Statement,
    StringType,
    Value,
)
from tested.testsuite import MainInput


def convert_value(value: Value) -> str:
    assert isinstance(value, StringType), f"Invalid literal: {value!r}"
    if value.type in (AdvancedStringTypes.CHAR, BasicStringTypes.TEXT):
        return shlex.quote(value.data)
    elif value.type == BasicStringTypes.UNKNOWN:
        return convert_unknown_type(value)
    raise AssertionError(f"Invalid literal: {value!r}")


def has_nested_function_call(fun: FunctionCall) -> bool:
    return len(list(filter(lambda a: isinstance(a, FunctionCall), fun.arguments))) > 0


def convert_function_call_nested(
    function: FunctionCall,
    index_fun: Callable[[], int],
    index_map: list,
    index=int,
) -> str:
    index_map.append({})
    result = ""
    for i, argument in enumerate(function.arguments):
        if (
            isinstance(argument, FunctionCall)
            and argument.type == FunctionType.FUNCTION
        ):
            # Delegate to the function template for function calls.
            index_map[-1][i] = index_fun()
            if has_nested_function_call(argument):
                result += (
                    convert_function_call_nested(
                        argument, index_fun, index_map, index_map[-1][i]
                    )
                    + "\n"
                )
            else:
                result += f"local ARG{index_map[-1][i]}=$({convert_function_call(argument, index_fun, index_map)})\n"

    result += f"local ARG{index}=$({function.name} "

    if function.type != FunctionType.PROPERTY:
        for i, argument in enumerate(function.arguments):
            if isinstance(argument, Identifier):
                # If the expression is an identifier, just echo it.
                result += f'"${argument}" '
            elif isinstance(argument, FunctionCall):
                # Delegate to the function template for function calls.
                if argument.type == FunctionType.PROPERTY:
                    result += (
                        convert_function_call(argument, index_fun, index_map) + "\n"
                    )
                else:
                    result += f'"$ARG{index_map[-1][i]}" '
            elif isinstance(argument, Value):
                # We have a value, delegate to the value template.
                result += convert_value(argument) + " "
    result += ")"
    index_map.pop()
    return result


def convert_function_call(
    function: FunctionCall,
    index_fun: Callable[[], int],
    index_map: list,
) -> str:
    index_map.append({})
    result = ""
    for i, argument in enumerate(function.arguments):
        if (
            isinstance(argument, FunctionCall)
            and argument.type == FunctionType.FUNCTION
        ):
            # Delegate to the function template for function calls.
            index_map[-1][i] = index_fun()
            if has_nested_function_call(argument):
                result += (
                    convert_function_call_nested(
                        argument, index_fun, index_map, index_map[-1][i]
                    )
                    + "\n"
                )
            else:
                result += f"local ARG{index_map[-1][i]}=$({convert_function_call(argument, index_fun, index_map)})\n"

    if function.type == FunctionType.PROPERTY:
        result += '"$'

    result += function.name

    if function.type != FunctionType.PROPERTY:
        result += " "
        for i, argument in enumerate(function.arguments):
            if isinstance(argument, Identifier):
                # If the expression is an identifier, just echo it.
                result += f'"${argument}" '
            elif isinstance(argument, FunctionCall):
                # Delegate to the function template for function calls.
                if argument.type == FunctionType.PROPERTY:
                    result += (
                        convert_function_call(argument, index_fun, index_map) + "\n"
                    )
                else:
                    result += f'"$ARG{index_map[-1][i]}" '
            elif isinstance(argument, Value):
                # We have a value, delegate to the value template.
                result += convert_value(argument) + " "
    else:
        result += '"'
    index_map.pop()
    return result


def unique_index_function():
    index = [-1]

    def get():
        index[0] += 1
        return index[0]

    return get


def convert_statement(statement: Statement) -> str:
    if isinstance(statement, Identifier):
        return f'"${statement}"'
    elif isinstance(statement, FunctionCall):
        index_fun = unique_index_function()
        return convert_function_call(statement, index_fun, [])
    elif isinstance(statement, Value):
        return convert_value(statement)
    elif isinstance(statement, Assignment):
        result = f"local {statement.variable}="
        if isinstance(statement.expression, FunctionCall):
            result += f"$({convert_statement(statement.expression)})"
        else:
            result += convert_statement(statement.expression)
    raise AssertionError(f"Unknown statement: {statement!r}")


indent = " " * 4


def convert_execution_unit(pu: PreparedExecutionUnit) -> str:
    result = f"""
function write_context_separator {{
    echo -n "--{pu.context_separator_secret}-- SEP" >>{pu.value_file}
    echo -n "--{pu.context_separator_secret}-- SEP" >>{pu.exception_file}
    echo -n "--{pu.context_separator_secret}-- SEP"
    echo -n "--{pu.context_separator_secret}-- SEP" >&2
}}

function write_separator {{
    echo -n "--{pu.testcase_separator_secret}-- SEP" >>{pu.value_file}
    echo -n "--{pu.testcase_separator_secret}-- SEP" >>{pu.exception_file}
    echo -n "--{pu.testcase_separator_secret}-- SEP"
    echo -n "--{pu.testcase_separator_secret}-- SEP" >&2
}}

function json_escape {{
    printf '%s' "$1" | python -c 'import json,sys; print(json.dumps(sys.stdin.read()))'
}}

function send_value {{
    echo -n "{{\\"type\\": \\"text\\", \\"data\\": $(json_escape "$1")}}" >>{pu.value_file}
}}

touch {pu.value_file} {pu.exception_file}

"""

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        result += f"function context_{i} {{\n"
        result += indent + ctx.before + "\n"

        # Import the submission if there is no main call.
        if not ctx.context.has_main_testcase():
            result += f"{indent}source {pu.submission_name}.sh\n"

        # Generate code for each testcase
        tc: PreparedTestcase
        for tc in ctx.testcases:
            result += indent + "write_separator\n"

            # Prepare command arguments if needed.
            if tc.testcase.is_main_testcase():
                assert isinstance(tc.input, MainInput)
                result += f"{indent}source {pu.submission_name}.sh "
                result += " ".join(shlex.quote(s) for s in tc.input.arguments) + "\n"
            else:
                assert isinstance(tc.input, PreparedTestcaseStatement)
                result += indent + convert_statement(tc.input.input_statement()) + "\n"
            result += "\n"

        result += indent + ctx.after + "\n"
        result += "}\n"

    for i, ctx in enumerate(pu.contexts):
        result += "write_context_separator\n"
        result += f"context_{i}\n"

    result += "exit $?\n"

    return result


# TODO: don't call Python to encode JSON in Bash.
#   Use something like jq.
def convert_encoder(values: list[Value]) -> str:
    result = f"""
function json_escape {{
    printf '%s' "$1" | python -c 'import json,sys; print(json.dumps(sys.stdin.read()))'
}}

function send_value {{
    echo "{{\\"type\\": \\"text\\", \\"data\\": $(json_escape "$1")}}␞"
}}
"""
    for value in values:
        index_fun = unique_index_function()
        function_call = FunctionCall(
            type=FunctionType.FUNCTION, name="send_value", arguments=[value]
        )
        result += f"{convert_function_call(function_call, index_fun, [])}\n"
    return result
