import json
from typing import Literal

from tested.datatypes import (
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    resolve_to_basic,
)
from tested.languages.preparation import (
    PreparedContext,
    PreparedExecutionUnit,
    PreparedFunctionCall,
    PreparedTestcase,
    PreparedTestcaseStatement,
)
from tested.languages.utils import convert_unknown_type, is_special_void_call
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
    VariableType,
    as_basic_type,
)
from tested.testsuite import MainInput


def convert_arguments(arguments: list[Expression | NamedArgument]) -> str:
    results = []
    for arg in arguments:
        if isinstance(arg, NamedArgument):
            results.append(f"{arg.name}: {convert_statement(arg.value)}")
        else:
            results.append(convert_statement(arg))
    return ", ".join(results)


def convert_value(value: Value) -> str:
    # Handle some advanced types.
    if value.type == AdvancedSequenceTypes.ARRAY:
        assert isinstance(value, SequenceType)
        return f"new {convert_declaration(value.type, value)}{{{convert_arguments(value.data)}}}"  # pyright: ignore
    elif value.type == AdvancedSequenceTypes.TUPLE:
        assert isinstance(value, SequenceType)
        return f"({convert_arguments(value.data)})"  # pyright: ignore
    elif value.type == AdvancedNumericTypes.SINGLE_PRECISION:
        if not isinstance(value.data, SpecialNumbers):
            return f"{value.data}f"
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return "Single.NaN"
        elif value.data == SpecialNumbers.POS_INFINITY:
            return "Single.PositiveInfinity"
        else:
            assert value.data == SpecialNumbers.NEG_INFINITY
            return f"Single.NegativeInfinity"
    elif value.type == AdvancedNumericTypes.U_INT_8:
        return f"(byte) {value.data}"
    elif value.type == AdvancedNumericTypes.INT_8:
        return f"(sbyte) {value.data}"
    elif value.type == AdvancedNumericTypes.INT_16:
        return f"(short) {value.data}"
    elif value.type == AdvancedNumericTypes.U_INT_16:
        return f"(ushort) {value.data}"
    elif value.type == AdvancedNumericTypes.INT_32:
        return f"(int) {value.data}"
    elif value.type == AdvancedNumericTypes.U_INT_32:
        return f"(uint) {value.data}"
    elif value.type == AdvancedNumericTypes.INT_64:
        return f"(long) {value.data}"
    elif value.type == AdvancedNumericTypes.U_INT_64:
        return f"(ulong) {value.data}"
    elif value.type == AdvancedNumericTypes.BIG_INT:
        return f'BigInteger.Parse("{value.data}")'
    # Handle basic types
    value = as_basic_type(value)
    if value.type == BasicNumericTypes.INTEGER:
        return str(value.data)
    elif value.type == BasicNumericTypes.REAL:
        if not isinstance(value.data, SpecialNumbers):
            return str(value.data)
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return "Double.NaN"
        elif value.data == SpecialNumbers.POS_INFINITY:
            return "Double.PositiveInfinity"
        else:
            assert SpecialNumbers.NEG_INFINITY
            return "Double.NegativeInfinity"
    elif value.type == BasicStringTypes.TEXT:
        return json.dumps(value.data)
    elif value.type == BasicBooleanTypes.BOOLEAN:
        return str(value.data).lower()
    elif value.type == BasicNothingTypes.NOTHING:
        return "null"
    elif value.type in (BasicSequenceTypes.SEQUENCE, BasicSequenceTypes.SET):
        assert isinstance(value, SequenceType)
        return f"new {convert_declaration(value.type, value)}() {{{convert_arguments(value.data)}}}"  # pyright: ignore
    elif value.type == BasicObjectTypes.MAP:
        assert isinstance(value, ObjectType)
        result = f"new {convert_declaration(value.type, value)}() {{"
        for i, pair in enumerate(value.data):
            result += (
                f"{{{convert_statement(pair.key)}, {convert_statement(pair.value)}}}"
            )
            if i != len(value.data) - 1:
                result += ", "
        result += "}"
        return result
    elif value.type == BasicStringTypes.UNKNOWN:
        assert isinstance(value, StringType)
        return convert_unknown_type(value)
    raise AssertionError(f"Invalid literal: {value!r}")


def convert_function_call(function: FunctionCall) -> str:
    result = ""
    if function.type == FunctionType.CONSTRUCTOR:
        result += "new "
    if function.namespace and not (
        (isinstance(function, PreparedFunctionCall) and function.has_root_namespace)
        and function.type == FunctionType.CONSTRUCTOR
    ):
        result += convert_statement(function.namespace)
        if function.type != FunctionType.ARRAY_ACCESS:
            result += "."
    result += function.name
    args = convert_arguments(function.arguments)  # pyright: ignore
    if function.type == FunctionType.ARRAY_ACCESS:
        result += f"![(int) {args}]"
    elif function.type != FunctionType.PROPERTY:
        result += f"({args})"
    return result


def convert_declaration(
    tp: AllTypes | VariableType | Literal["Object"],
    value: Statement | None,
    nt=None,
    inner=False,
) -> str:
    def extract_type_tuple(type_, generic=True):
        if isinstance(type_, tuple):
            if generic:
                return type_
            else:
                return "Object", False
        else:
            return type_, False

    if isinstance(tp, VariableType):
        return tp.data
    elif tp == AdvancedSequenceTypes.ARRAY:
        # TODO: this is very complex, and why?
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, SequenceType)
            else nt
            if nt
            else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_, False)
        return convert_declaration(base_type, None, sub_type) + "[]"
    elif tp == AdvancedSequenceTypes.TUPLE:
        # TODO: rework this
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, SequenceType)
            else nt
            if nt
            else "Object"
        )
        assert isinstance(value, SequenceType)
        base_type, sub_type = extract_type_tuple(type_, False)
        converted_type = convert_declaration(base_type, None, sub_type)
        return "(" + ", ".join(converted_type for _ in range(len(value.data)))
    elif tp in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
        return "BigInteger" + ("?" if inner else "")
    elif tp == AdvancedNumericTypes.INT_8:
        return "Byte" + ("?" if inner else "")
    elif tp == AdvancedNumericTypes.U_INT_8:
        return "SByte" + ("?" if inner else "")
    elif tp == AdvancedNumericTypes.INT_16:
        return "Int16" + ("?" if inner else "")
    elif tp == AdvancedNumericTypes.U_INT_16:
        return "UInt16" + ("?" if inner else "")
    elif tp == AdvancedNumericTypes.U_INT_32:
        return "UInt32" + ("?" if inner else "")
    elif tp == AdvancedNumericTypes.INT_64:
        return "Int64" + ("?" if inner else "")
    elif tp == AdvancedNumericTypes.U_INT_64:
        return "UInt64" + ("?" if inner else "")
    elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
        return "Single" + ("?" if inner else "")
    elif tp == "Object":
        return tp
    basic = resolve_to_basic(tp)
    if basic == BasicSequenceTypes.SEQUENCE:
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, SequenceType)
            else nt
            if nt
            else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_)
        return f"List<{convert_declaration(base_type, None, sub_type)}>"
    elif basic == BasicSequenceTypes.SET:
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, SequenceType)
            else nt
            if nt
            else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_)
        return f"Set<{convert_declaration(base_type, None, sub_type)}>"
    elif basic == BasicBooleanTypes.BOOLEAN:
        return "Boolean" + ("?" if inner else "")
    elif basic == BasicStringTypes.TEXT:
        return "string" + ("?" if inner else "")
    elif basic == BasicNumericTypes.INTEGER:
        return "Int32" + ("?" if inner else "")
    elif basic == BasicNumericTypes.REAL:
        return "Double" + ("?" if inner else "")
    elif basic == BasicObjectTypes.MAP:
        if isinstance(value, ObjectType):
            key_type_ = value.get_key_type() or "Object"
            value_type_ = value.get_value_type() or "Object"
        elif nt:
            key_type_, value_type_ = nt
        else:
            key_type_, value_type_ = "Object", "Object"
        key_base_type, key_sub_type = extract_type_tuple(key_type_)
        value_base_type, value_sub_type = extract_type_tuple(value_type_)
        return f"Dictionary<{convert_declaration(key_base_type, None, key_sub_type)}, {convert_declaration(value_base_type, None, value_sub_type)}>"
    elif basic in (BasicNothingTypes.NOTHING, BasicStringTypes.ANY):
        return "Object"
    raise AssertionError(f"Unknown type: {tp!r}")


def convert_statement(statement: Statement, full=False) -> str:
    if isinstance(statement, Identifier):
        return statement
    elif isinstance(statement, FunctionCall):
        return convert_function_call(statement)
    elif isinstance(statement, Value):
        return convert_value(statement)
    elif isinstance(statement, Assignment):
        if full:
            prefix = convert_declaration(statement.type, statement.expression)
        else:
            prefix = ""
        return (
            f"{prefix}{statement.variable} = "
            f"{convert_statement(statement.expression)}"
        )
    raise AssertionError(f"Unknown statement: {statement!r}")


def _generate_internal_context(ctx: PreparedContext, pu: PreparedExecutionUnit) -> str:
    result = ctx.before + "\n"

    # Generate code for each testcase
    tc: PreparedTestcase
    for tc in ctx.testcases:
        result += "WriteSeparator();\n"

        # Make a variable available outside of the try-catch block.
        if (
            not tc.testcase.is_main_testcase()
            and isinstance(tc.input, PreparedTestcaseStatement)
            and isinstance(tc.input.statement, Assignment)
        ):
            result += (
                convert_declaration(
                    tc.input.statement.type, tc.input.statement.expression, inner=True
                )
                + " "
            )
            result += tc.input.statement.variable + " = null;\n"

        result += "try {\n"
        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            result += " " * 4 + f"{pu.submission_name}.Main(new string[]{{"
            wrapped = [json.dumps(a) for a in tc.input.arguments]
            result += ", ".join(wrapped)
            result += "});\n"
        else:
            assert isinstance(tc.input, PreparedTestcaseStatement)
            if is_special_void_call(tc.input, pu.language):
                # The method has a "void" return type, so don't wrap it.
                result += (
                    " " * 4
                    + convert_statement(tc.input.unwrapped_input_statement())
                    + ";\n"
                )
                result += " " * 4 + convert_statement(tc.input.no_value_call()) + ";\n"
            else:
                result += (
                    " " * 4 + convert_statement(tc.input.input_statement()) + ";\n"
                )
        result += " " * 4 + convert_statement(tc.exception_statement()) + ";\n"
        result += "} catch (System.Exception E) {\n"
        result += " " * 4 + convert_statement(tc.exception_statement("e")) + ";\n"
        result += "}\n"

    result += ctx.after

    return result


def convert_execution_unit(pu: PreparedExecutionUnit) -> str:
    result = f"""
using System;
using System.IO;

namespace Tested
{{

class {pu.unit.name}
{{
    private readonly StreamWriter valueFile;
    private readonly StreamWriter exceptionFile;
    private readonly StreamWriter stdout;
    private readonly StreamWriter stderr;

    public {pu.unit.name}()
    {{
        valueFile = new StreamWriter(File.OpenWrite("{pu.value_file}"));
        exceptionFile = new StreamWriter(File.OpenWrite("{pu.exception_file}"));
        stdout = new StreamWriter(Console.OpenStandardOutput());
        stderr = new StreamWriter(Console.OpenStandardError());

        stdout.AutoFlush = true;
        stderr.AutoFlush = true;
        Console.SetOut(stdout);
        Console.SetError(stderr);
    }}
    
    private void WriteSeparator()
    {{
        valueFile.Write("--{pu.testcase_separator_secret}-- SEP");
        exceptionFile.Write("--{pu.testcase_separator_secret}-- SEP");
        stdout.Write("--{pu.testcase_separator_secret}-- SEP");
        stderr.Write("--{pu.testcase_separator_secret}-- SEP");
    }}
    
    private void WriteContextSeparator()
    {{
        valueFile.Write("--{pu.context_separator_secret}-- SEP");
        exceptionFile.Write("--{pu.context_separator_secret}-- SEP");
        stdout.Write("--{pu.context_separator_secret}-- SEP");
        stderr.Write("--{pu.context_separator_secret}-- SEP");
    }}
    
    private void SendValue(object? value)
    {{
        Values.WriteValue(valueFile, value);
    }}
    
    private void SendException(Exception? e)
    {{
        Values.WriteException(exceptionFile, e);
    }}

    private void SendSpecificValue(EvaluationResult value)
    {{
        Values.SendEvaluated(valueFile, value);
    }}

    private void SendSpecificException(EvaluationResult exception)
    {{
        Values.SendEvaluated(exceptionFile, exception);
    }}
    """

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        result += f"""
    private void Context{i}() {{
        {_generate_internal_context(ctx, pu)}
    }}
    """

    result += " " * 4 + "void Execute()\n    {\n"
    for i, ctx in enumerate(pu.contexts):
        result += f"""
            WriteContextSeparator();
            Context{i}();
        """
    result += """
        valueFile.Close();
        exceptionFile.Close();
    }
    """

    result += f"""        
    public static void Main(string[] a) {{
        {pu.unit.name} execution = new {pu.unit.name}();
        execution.Execute();
    }}
}}
}}
    """

    return result


def convert_selector(contexts: list[str]) -> str:
    result = """
    using System;
    namespace Tested
    {
    class Selector
    {
        public static void Main(string[] a)
        {
            string name = a[0];
    """
    for c in contexts:
        result += f"""
                if ("{c}" == name) {{
                    {c}.Main(Array.Empty<string>());
                }}
        """
    result += "}\n}\n}"
    return result


def convert_check_function(function: FunctionCall) -> str:
    return f"""
    using System;
    using System.IO;
    using Tested;
    
    namespace Tested
    {{
        public class EvaluatorExecutor
        {{
            public static void Main(string[] args)
            {{
                var result = {convert_function_call(function)};
                StreamWriter writer = new StreamWriter(Console.OpenStandardOutput());
                writer.AutoFlush = true;
                Console.SetOut(writer);
                Values.SendEvaluated(writer, result);
            }}
        }}
    }}
    """


def convert_encoder(values: list[Value]) -> str:
    result = """
    using Tested;
    using System.Numerics;
    
    namespace Tested
    {
      public class Encode
      {
        public static void Main(string[] args)
        {
          StreamWriter stdout = new StreamWriter(Console.OpenStandardOutput());
          stdout.AutoFlush = true;
          Console.SetOut(stdout);
    """

    for value in values:
        result += " " * 6 + f"Values.WriteValue(stdout, {convert_value(value)});"
        result += " " * 6 + 'stdout.Write("␞");\n'

    result += "}\n}\n}\n"
    return result
