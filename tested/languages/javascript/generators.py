import json
from typing import List, cast

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
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
    ObjectType,
    SequenceType,
    SpecialNumbers,
    Statement,
    StringType,
    Value,
    as_basic_type,
)
from tested.testsuite import MainInput


def convert_arguments(arguments: List[Expression]) -> str:
    return ", ".join(convert_statement(arg, True) for arg in arguments)


def convert_value(value: Value) -> str:
    # Handle some advanced types.
    if value.type == AdvancedNothingTypes.UNDEFINED:
        return "undefined"
    elif value.type == AdvancedNumericTypes.DOUBLE_EXTENDED:
        raise AssertionError("Double extended values are not supported in js.")
    elif value.type == AdvancedNumericTypes.FIXED_PRECISION:
        raise AssertionError("Fixed precision values are not supported in js.")
    elif value.type in (
        AdvancedNumericTypes.INT_64,
        AdvancedNumericTypes.U_INT_64,
        AdvancedNumericTypes.BIG_INT,
    ):
        return f'BigInt("{value.data}")'
    # Handle basic types
    value = as_basic_type(value)
    if value.type in (BasicNumericTypes.INTEGER, BasicNumericTypes.REAL):
        if not isinstance(value.data, SpecialNumbers):
            return str(value.data)
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return "NaN"
        elif value.data == SpecialNumbers.POS_INFINITY:
            return "Infinity"
        else:
            return "(-Infinity)"
    elif value.type == BasicStringTypes.TEXT:
        return json.dumps(value.data)
    elif value.type == BasicBooleanTypes.BOOLEAN:
        return str(value.data).lower()
    elif value.type == BasicNothingTypes.NOTHING:
        return "null"
    elif value.type == BasicSequenceTypes.SEQUENCE:
        assert isinstance(value, SequenceType)
        return f"[{convert_arguments(value.data)}]"
    elif value.type == BasicSequenceTypes.SET:
        assert isinstance(value, SequenceType)
        return f"new Set([{convert_arguments(value.data)}])"
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


def convert_function_call(call: FunctionCall, internal=False) -> str:
    result = ""
    if not internal:
        result += "await "
    if call.type == FunctionType.CONSTRUCTOR:
        result += "new "
    if call.namespace:
        result += convert_statement(call.namespace, True) + "."
    result += call.name
    if call.type != FunctionType.PROPERTY:
        result += f"({convert_arguments(cast(List[Expression], call.arguments))})"
    return result


def convert_statement(statement: Statement, internal=False, full=False) -> str:
    if isinstance(statement, Identifier):
        return statement
    elif isinstance(statement, FunctionCall):
        return convert_function_call(statement, internal)
    elif isinstance(statement, Value):
        return convert_value(statement)
    elif isinstance(statement, Assignment):
        if full:
            prefix = "let "
        else:
            prefix = ""
        return (
            f"{prefix}{statement.variable} = "
            f"{convert_statement(statement.expression, True)}"
        )
    raise AssertionError(f"Unknown statement: {statement!r}")


def _generate_internal_context(ctx: PreparedContext, pu: PreparedExecutionUnit) -> str:
    result = ctx.before + "\n"

    # Import the submission if there is no main call.
    if not ctx.context.has_main_testcase():
        result += f"""
            delete require.cache[require.resolve("./{pu.submission_name}.js")];
            const {pu.submission_name} = require("./{pu.submission_name}.js");
            """

    # Generate code for each testcase
    tc: PreparedTestcase
    for tc in ctx.testcases:
        result += "writeSeparator();\n"

        # Prepare command arguments if needed.
        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            wrapped = [json.dumps(a) for a in tc.input.arguments]
            result += f"""
            let new_args = [process.argv[0]];
            new_args = new_args.concat([{", ".join(wrapped)}]);
            process.argv = new_args;
            """

        # We need special code to make variables available outside of the try-catch block.
        if (
            not tc.testcase.is_main_testcase()
            and isinstance(tc.input, PreparedTestcaseStatement)
            and isinstance(tc.input.statement, Assignment)
        ):
            result += f"let {tc.input.statement.variable}\n"

        result += "try {\n"
        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            result += f"""
                delete require.cache[require.resolve("./{pu.submission_name}.js")];
                const {pu.submission_name} = require("./{pu.submission_name}.js");
            """
        else:
            assert isinstance(tc.input, PreparedTestcaseStatement)
            result += " " * 4 + convert_statement(tc.input.input_statement()) + ";\n"

        result += f"""
            {convert_statement(tc.exception_statement())};
        }} catch(e) {{
            {convert_statement(tc.exception_statement("e"))};
        }}
        """

    result += ctx.after

    return result


def convert_execution_unit(pu: PreparedExecutionUnit) -> str:
    result = """
    const fs = require('fs');
    const values = require("./values.js");
    """

    # Import the language specific functions we will need.
    for name in pu.evaluator_names:
        result += f'const {name} = require("./{name}.js");\n'

    # We now open files for results and define some functions.
    result += f"""
    const valueFile = fs.openSync("{pu.value_file}", "w");
    const exceptionFile = fs.openSync("{pu.exception_file}", "w");
    
    function writeSeparator() {{
        fs.writeSync(valueFile, "--{pu.testcase_separator_secret}-- SEP");
        fs.writeSync(exceptionFile, "--{pu.testcase_separator_secret}-- SEP");
        fs.writeSync(process.stdout.fd, "--{pu.testcase_separator_secret}-- SEP");
        fs.writeSync(process.stderr.fd, "--{pu.testcase_separator_secret}-- SEP");
    }}
    
    function writeContextSeparator() {{
        fs.writeSync(valueFile, "--{pu.context_separator_secret}-- SEP");
        fs.writeSync(exceptionFile, "--{pu.context_separator_secret}-- SEP");
        fs.writeSync(process.stdout.fd, "--{pu.context_separator_secret}-- SEP");
        fs.writeSync(process.stderr.fd, "--{pu.context_separator_secret}-- SEP");
    }}
    
    async function sendValue(value) {{
        values.sendValue(valueFile, await value);
    }}

    async function sendException(exception) {{
        values.sendException(exceptionFile, await exception);
    }}

    async function sendSpecificValue(value) {{
        values.sendEvaluated(valueFile, await value);
    }}

    async function sendSpecificException(exception) {{
        values.sendEvaluated(exceptionFile, await exception);
    }}
    """

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        result += f"""
        async function context{i}() {{
            {_generate_internal_context(ctx, pu)}
        }}
        """

    # Functions to write separators
    result += f"(async () => {{\n"

    for i, ctx in enumerate(pu.contexts):
        result += f"""
            writeContextSeparator();
            await context{i}();
        """

    result += """
        fs.closeSync(valueFile);
        fs.closeSync(exceptionFile);
    })();
    """

    return result


def convert_check_function(evaluator: str, function: FunctionCall) -> str:
    return f"""
    (async () => {{
        const {evaluator} = require('./{evaluator}.js');
        const values = require('./values.js');

        const result = {convert_function_call(function)};
        values.sendEvaluated(process.stdout.fd, result);
    }})();
    """


def convert_encoder(values: List[Value]) -> str:
    result = """
    const values = require('./values.js');
    const fs = require("fs");
    """

    for value in values:
        result += f"values.sendValue(process.stdout.fd, {convert_value(value)});\n"
        result += 'fs.writeSync(process.stdout.fd, "\\n");\n'

    return result
