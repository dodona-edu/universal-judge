import json

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
    AdvancedStringTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    resolve_to_basic,
)
from tested.datatypes.advanced import AdvancedObjectTypes
from tested.languages.conventionalize import submission_file
from tested.languages.preparation import (
    PreparedContext,
    PreparedExecutionUnit,
    PreparedTestcase,
    PreparedTestcaseStatement,
)
from tested.languages.utils import convert_unknown_type
from tested.serialisation import (
    Expression,
    FunctionCall,
    FunctionType,
    Identifier,
    NamedArgument,
    ObjectType,
    PropertyAssignment,
    SequenceType,
    SpecialNumbers,
    Statement,
    StringType,
    Value,
    VariableAssignment,
    VariableType,
    as_basic_type,
)
from tested.testsuite import MainInput


def convert_arguments(arguments: list[NamedArgument | Expression]) -> str:
    results = []
    for arg in arguments:
        if isinstance(arg, NamedArgument):
            results.append(f"{arg.name}={convert_statement(arg.value, True)}")
        else:
            results.append(convert_statement(arg, True))
    return ", ".join(results)


def convert_value(value: Value) -> str:
    # Handle some advanced types.
    if value.type == AdvancedNothingTypes.UNDEFINED:
        return "undefined"
    elif value.type == AdvancedNumericTypes.DOUBLE_EXTENDED:
        raise AssertionError("Double extended values are not supported in ts.")
    elif value.type == AdvancedNumericTypes.FIXED_PRECISION:
        raise AssertionError("Fixed precision values are not supported in ts.")
    elif value.type == AdvancedObjectTypes.OBJECT:
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
        return json.dumps(value.data, ensure_ascii=False)
    elif value.type == BasicBooleanTypes.BOOLEAN:
        return str(value.data).lower()
    elif value.type == BasicNothingTypes.NOTHING:
        return "null"
    elif value.type == BasicSequenceTypes.SEQUENCE:
        assert isinstance(value, SequenceType)
        return f"[{convert_arguments(value.data)}]"  # pyright: ignore
    elif value.type == BasicSequenceTypes.SET:
        assert isinstance(value, SequenceType)
        return f"new Set([{convert_arguments(value.data)}])"  # pyright: ignore
    elif value.type == BasicObjectTypes.MAP:
        assert isinstance(value, ObjectType)
        result = "new Map(["
        for i, pair in enumerate(value.data):
            result += "["
            result += convert_statement(pair.key, True)
            result += ", "
            result += convert_statement(pair.value, True)
            result += "]"
            if i != len(value.data) - 1:
                result += ", "
        result += "])"
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
        result += f"({convert_arguments(call.arguments)})"  # pyright: ignore
    return result


def convert_declaration(statement: Statement, tp: AllTypes | VariableType) -> str:
    if isinstance(tp, VariableType):
        return f"{tp.data}"
    elif tp == AdvancedStringTypes.CHAR:
        return "string"
    elif isinstance(tp, AdvancedNumericTypes):
        return "number"
    elif tp == AdvancedNothingTypes.NULL:
        return "null"
    elif tp == AdvancedNothingTypes.UNDEFINED:
        return "undefined"

    basic = resolve_to_basic(tp)
    if basic == BasicBooleanTypes.BOOLEAN:
        return "boolean"
    elif basic == BasicStringTypes.TEXT:
        return "string"
    elif basic == BasicNumericTypes.INTEGER:
        return "number"
    elif basic == BasicNumericTypes.REAL:
        return "number"
    elif basic == BasicNothingTypes.NOTHING:
        return "null"
    elif basic == BasicObjectTypes.MAP:
        return "object"
    elif basic == BasicSequenceTypes.SEQUENCE:
        type_ = "Object"

        if isinstance(statement, VariableAssignment):
            expression = statement.expression
        else:
            expression = statement
        if isinstance(expression, SequenceType):
            type_ = {
                (
                    convert_declaration(element, element.type)
                    if not isinstance(element, Identifier)
                    and not isinstance(element, FunctionCall)
                    else "id"
                )
                for element in expression.data
            }
            if "id" in type_:
                type_ = "any"
            else:
                type_ = "|".join(type_)

        return f"Array<{type_}>"
    elif basic == BasicSequenceTypes.SET:
        type_ = "Object"

        if isinstance(statement, VariableAssignment):
            expression = statement.expression
        else:
            expression = statement
        if isinstance(expression, SequenceType):
            type_ = {
                (
                    convert_declaration(element, element.type)
                    if not isinstance(element, Identifier)
                    and not isinstance(element, FunctionCall)
                    else "id"
                )
                for element in expression.data
            }
            if "id" in type_:
                type_ = "any"
            else:
                type_ = "|".join(type_)

        return f"Set<{type_}>"
    raise AssertionError(f"Unknown type: {tp!r}")


def convert_statement(statement: Statement, internal=False, full=False) -> str:
    if isinstance(statement, Identifier):
        return statement
    elif isinstance(statement, FunctionCall):
        return convert_function_call(statement, internal)
    elif isinstance(statement, Value):
        return convert_value(statement)
    elif isinstance(statement, PropertyAssignment):
        return (
            f"{convert_statement(statement.property, True)} = "
            f"{convert_statement(statement.expression, True)}"
        )
    elif isinstance(statement, VariableAssignment):
        if full:
            return (
                f"let {statement.variable} : {convert_declaration(statement, statement.type)} = "
                f"{convert_statement(statement.expression, True)}"
            )

        return (
            f"{statement.variable} = "
            f"{convert_statement(statement.expression, True)}"
        )
    raise AssertionError(f"Unknown statement: {statement!r}")


def _generate_internal_context(ctx: PreparedContext, pu: PreparedExecutionUnit) -> str:
    result = ctx.before + "\n"

    # Import the submission if there is no main call.
    if not ctx.context.has_main_testcase():
        result += f"""
            writeSeparator();
            delete require.cache[require.resolve("./{submission_file(pu.language)}")];
            let {pu.submission_name} = await import('./{submission_file(pu.language)}');
            """

    # Generate code for each testcase
    tc: PreparedTestcase
    for i, tc in enumerate(ctx.testcases):
        # Prepare command arguments if needed.
        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            wrapped = [json.dumps(a) for a in tc.input.arguments]
            result += f"""
            writeSeparator();
            let new_args = [process.argv[0]];
            new_args = new_args.concat([{", ".join(wrapped)}]);
            process.argv = new_args;
            """
        elif i != 0:
            result += "writeSeparator();\n"

        # We need special code to make variables available outside of the try-catch block.
        if (
            not tc.testcase.is_main_testcase()
            and isinstance(tc.input, PreparedTestcaseStatement)
            and isinstance(tc.input.statement, VariableAssignment)
        ):
            result += f"let {tc.input.statement.variable}\n"

        result += "try {\n"
        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            result += f"""
                delete require.cache[require.resolve("./{pu.submission_name}.ts")];
                let {pu.submission_name} = await import('./{pu.submission_name}.ts');
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
    result = f"""
    import * as fs from 'fs';
    import * as values from './values.ts';
    """

    # Import the language specific functions we will need.
    for name in pu.evaluator_names:
        result += f"import * as {name} from './{name}.ts';\n"

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

    async function sendValue(value: unknown) {{
        values.sendValue(valueFile, await value);
    }}

    async function sendException(exception: unknown) {{
        values.sendException(exceptionFile, await exception);
    }}

    async function sendSpecificValue(value: unknown) {{
        values.sendEvaluated(valueFile, await value);
    }}

    async function sendSpecificException(exception: unknown) {{
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

    result += f"""
        fs.closeSync(valueFile);
        fs.closeSync(exceptionFile);
    }})();
    """

    return result


def convert_check_function(evaluator: str, function: FunctionCall) -> str:
    return f"""
    (async () => {{
        import * as {evaluator} from './{evaluator}.ts';
        import * as values from './values.ts';

        const result = {convert_function_call(function)};
        values.sendEvaluated(process.stdout.fd, result);
    }})();
    """


def convert_encoder(values: list[Value]) -> str:
    result = f"""
    import * as values from './values.ts';
    import * as fs from 'fs';
    """

    for value in values:
        result += f"values.sendValue(process.stdout.fd, {convert_value(value)});\n"
        result += f'fs.writeSync(process.stdout.fd, "␞");\n'

    return result
