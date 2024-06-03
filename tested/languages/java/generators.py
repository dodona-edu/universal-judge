import json
from typing import Literal, cast

from tested.datatypes import (
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
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
from tested.languages.preparation import (
    PreparedContext,
    PreparedExecutionUnit,
    PreparedFunctionCall,
    PreparedTestcase,
    PreparedTestcaseStatement,
)
from tested.languages.utils import convert_unknown_type, is_special_void_call
from tested.serialisation import (
    Expression,
    FunctionCall,
    FunctionType,
    Identifier,
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


def convert_arguments(arguments: list[Expression]) -> str:
    return ", ".join(convert_statement(arg) for arg in arguments)


def convert_value(value: Value) -> str:
    # Handle some advanced types.
    if value.type == AdvancedSequenceTypes.ARRAY:
        assert isinstance(value, SequenceType)
        return f"new {convert_declaration(value.type, value)}{{{convert_arguments(value.data)}}}"
    elif value.type == AdvancedNumericTypes.SINGLE_PRECISION:
        if not isinstance(value.data, SpecialNumbers):
            return f"{value.data}f"
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return "Float.NaN"
        elif value.data == SpecialNumbers.POS_INFINITY:
            return "Float.POSITIVE_INFINITY"
        else:
            assert value.data == SpecialNumbers.NEG_INFINITY
            return f"Float.NEGATIVE_INFINITY"
    elif value.type == AdvancedNumericTypes.INT_8:
        return f"(byte) {value.data}"
    elif value.type in (AdvancedNumericTypes.U_INT_8, AdvancedNumericTypes.INT_16):
        return f"(short) {value.data}"
    elif value.type in (AdvancedNumericTypes.U_INT_16, AdvancedNumericTypes.INT_32):
        return str(value.data)
    elif value.type in (AdvancedNumericTypes.U_INT_32, AdvancedNumericTypes.INT_64):
        return f"{value.data}L"
    elif value.type in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
        return f'new BigInteger("{value.data}")'
    elif value.type in (
        AdvancedNumericTypes.DOUBLE_EXTENDED,
        AdvancedNumericTypes.FIXED_PRECISION,
    ):
        if not isinstance(value.data, SpecialNumbers):
            return f'new BigDecimal("{value.data}")'
        else:
            raise AssertionError("Special numbers not supported for BigDecimal")
    elif value.type == AdvancedStringTypes.CHAR:
        assert isinstance(value, StringType)
        return "'" + value.data.replace("'", "\\'") + "'"
    # Handle basic types
    value = as_basic_type(value)
    if value.type == BasicNumericTypes.INTEGER:
        # Basic heuristic for long numbers
        if (value.data > (2**31 - 1)) or (value.data < -(2**31)):
            return f"{value.data}L"
        else:
            return str(value.data)
    elif value.type == BasicNumericTypes.REAL:
        if not isinstance(value.data, SpecialNumbers):
            return str(value.data)
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return "Double.NaN"
        elif value.data == SpecialNumbers.POS_INFINITY:
            return "Double.POSITIVE_INFINITY"
        else:
            assert SpecialNumbers.NEG_INFINITY
            return "Double.NEGATIVE_INFINITY"
    elif value.type == BasicStringTypes.TEXT:
        return json.dumps(value.data)
    elif value.type == BasicBooleanTypes.BOOLEAN:
        return str(value.data).lower()
    elif value.type == BasicNothingTypes.NOTHING:
        return "null"
    elif value.type == BasicSequenceTypes.SEQUENCE:
        assert isinstance(value, SequenceType)
        return f"List.of({convert_arguments(value.data)})"
    elif value.type == BasicSequenceTypes.SET:
        assert isinstance(value, SequenceType)
        return f"Set.of({convert_arguments(value.data)})"
    elif value.type == BasicObjectTypes.MAP:
        assert isinstance(value, ObjectType)
        result = "Map.ofEntries("
        for i, pair in enumerate(value.data):
            result += "Map.entry("
            result += convert_statement(pair.key)
            result += ", "
            result += convert_statement(pair.value)
            result += ")"
            if i != len(value.data) - 1:
                result += ", "
        result += ")"
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
        result += convert_statement(function.namespace) + "."
    result += function.name
    if function.type != FunctionType.PROPERTY:
        result += f"({convert_arguments(cast(list[Expression], function.arguments))})"
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
            else nt if nt else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_, False)
        return convert_declaration(base_type, None, sub_type, False) + "[]"
    elif tp in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
        return "BigInteger"
    elif tp in (
        AdvancedNumericTypes.DOUBLE_EXTENDED,
        AdvancedNumericTypes.FIXED_PRECISION,
    ):
        return "BigDecimal"
    elif tp == AdvancedNumericTypes.INT_8:
        return "Byte" if inner else "byte"
    elif tp in (AdvancedNumericTypes.U_INT_8, AdvancedNumericTypes.INT_16):
        return "Short" if inner else "short"
    elif tp in (AdvancedNumericTypes.U_INT_16, AdvancedNumericTypes.INT_32):
        return "Integer" if inner else "int"
    elif tp in (AdvancedNumericTypes.U_INT_32, AdvancedNumericTypes.INT_64):
        return "Long" if inner else "long"
    elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
        return "Float" if inner else "float"
    elif tp == "Object":
        return tp
    elif tp == AdvancedStringTypes.CHAR:
        return "Character" if inner else "char"
    basic = resolve_to_basic(tp)
    if basic == BasicSequenceTypes.SEQUENCE:
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, SequenceType)
            else nt if nt else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_)
        return f"List<{convert_declaration(base_type, None, sub_type, True)}>"
    elif basic == BasicSequenceTypes.SET:
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, SequenceType)
            else nt if nt else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_)
        return f"Set<{convert_declaration(base_type, None, sub_type, True)}>"
    elif basic == BasicBooleanTypes.BOOLEAN:
        return "Boolean" if inner else "boolean"
    elif basic == BasicStringTypes.TEXT:
        return "String"
    elif basic == BasicNumericTypes.INTEGER:
        return "Integer" if inner else "int"
    elif basic == BasicNumericTypes.REAL:
        return "Double" if inner else "double"
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
        return f"Map<{convert_declaration(key_base_type, None, key_sub_type, True)}, {convert_declaration(value_base_type, None, value_sub_type, True)}>"
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
    elif isinstance(statement, PropertyAssignment):
        return (
            f"{convert_statement(statement.property)} = "
            f"{convert_statement(statement.expression)}"
        )
    elif isinstance(statement, VariableAssignment):
        if full:
            prefix = convert_declaration(statement.type, statement.expression) + " "
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
        result += "this.writeSeparator();\n"

        # In Java, we need special code to make variables available outside
        # the try-catch block.
        if (
            not tc.testcase.is_main_testcase()
            and isinstance(tc.input, PreparedTestcaseStatement)
            and isinstance(tc.input.statement, VariableAssignment)
        ):
            result += (
                convert_declaration(
                    tc.input.statement.type, tc.input.statement.expression
                )
                + " "
            )
            result += tc.input.statement.variable + " = null;\n"

        result += "try {\n"
        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            result += " " * 4 + f"{pu.submission_name}.main(new String[]{{"
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
        result += "} catch (Exception | AssertionError e) {\n"
        result += " " * 4 + convert_statement(tc.exception_statement("e")) + ";\n"
        result += "}\n"

    result += ctx.after

    return result


def convert_execution_unit(pu: PreparedExecutionUnit) -> str:
    result = f"""
    import java.io.*;
    import java.util.*;
    import java.util.function.*;
    import java.math.BigInteger;
    import java.math.BigDecimal;
    
    public class {pu.unit.name} implements Closeable {{
    
        private final PrintWriter valueWriter;
        private final PrintWriter exceptionWriter;
    
        public {pu.unit.name}() throws Exception {{
            this.valueWriter = new PrintWriter("{pu.value_file}");
            this.exceptionWriter = new PrintWriter("{pu.exception_file}");
        }}
        
        private void writeSeparator() throws Exception {{
            valueWriter.write("--{pu.testcase_separator_secret}-- SEP");
            exceptionWriter.write("--{pu.testcase_separator_secret}-- SEP");
            System.err.print("--{pu.testcase_separator_secret}-- SEP");
            System.out.print("--{pu.testcase_separator_secret}-- SEP");
            valueWriter.flush();
            exceptionWriter.flush();
            System.err.flush();
            System.out.flush();
        }}
        
        private void writeContextSeparator() throws Exception {{
            valueWriter.write("--{pu.context_separator_secret}-- SEP");
            exceptionWriter.write("--{pu.context_separator_secret}-- SEP");
            System.err.print("--{pu.context_separator_secret}-- SEP");
            System.out.print("--{pu.context_separator_secret}-- SEP");
            valueWriter.flush();
            exceptionWriter.flush();
            System.err.flush();
            System.out.flush();
        }}
        
        private void sendValue(Object value) throws Exception {{
            Values.send(valueWriter, value);
        }}
        
        private void sendException(Throwable exception) throws Exception {{
            Values.sendException(exceptionWriter, exception);
        }}

        private void sendSpecificValue(EvaluationResult value) {{
            Values.sendEvaluated(valueWriter, value);
        }}

        private void sendSpecificException(EvaluationResult exception) {{
            Values.sendEvaluated(exceptionWriter, exception);
        }}
    """

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        result += f"""
        private void context{i}() throws Exception {{
            {_generate_internal_context(ctx, pu)}
        }}
        """

    result += " " * 4 + "void execute() throws Exception {\n"
    for i, ctx in enumerate(pu.contexts):
        result += f"""
            this.writeContextSeparator();
            this.context{i}();
        """
    result += " " * 4 + "}\n"

    result += f"""
        @Override
        public void close() throws IOException {{
            this.valueWriter.close();
            this.exceptionWriter.close();
        }}
        
        public static void main(String[] a) throws Exception {{
            try({pu.unit.name} execution = new {pu.unit.name}()) {{
                execution.execute();
            }}
        }}
    }}
    """

    return result


def convert_selector(contexts: list[str]) -> str:
    result = """
    class Selector {
        public static void main(String[] a) throws Exception {
            var name = a[0];
    """
    for c in contexts:
        result += f"""
                if ("{c}".equals(name)) {{
                    {c}.main(new String[]{{}});
                }}
        """
    result += "}\n}"
    return result


def convert_check_function(function: FunctionCall) -> str:
    return f"""
    import java.util.*;
    import java.io.*;
    
    public class EvaluatorExecutor {{
    
        public static void main(String[] args) throws Exception {{
            var result = {convert_function_call(function)};
            PrintWriter writer = new PrintWriter(System.out);
            Values.sendEvaluated(writer, result);
            writer.flush();
        }}
    }}
    """


def convert_encoder(values: list[Value]) -> str:
    result = """
import java.util.*;
import java.io.*;
import java.math.BigInteger;
import java.math.BigDecimal;

public class Encode {
    public static void main(String[] args) throws Exception {
        PrintWriter writer = new PrintWriter(System.out);
"""

    for value in values:
        result += " " * 8 + f"Values.send(writer, {convert_value(value)});\n"
        result += " " * 8 + 'writer.write("␞");\n'

    result += " " * 8 + "writer.close();\n"
    result += " " * 4 + "}\n"
    result += "}\n"
    return result
