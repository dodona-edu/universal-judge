import json
from typing import List, Literal, Optional, Union

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
)
from tested.serialisation import (
    Assignment,
    Expression,
    FunctionCall,
    FunctionType,
    Identifier,
    NamedArgument,
    SpecialNumbers,
    Statement,
    Value,
    VariableType,
    as_basic_type,
)
from tested.utils import get_args


def convert_arguments(arguments: List[Expression | NamedArgument]) -> str:
    results = []
    for arg in arguments:
        if isinstance(arg, NamedArgument):
            results.append(f"{arg.name}={convert_statement(arg.value)}")
        else:
            results.append(convert_statement(arg))
    return ", ".join(results)


def convert_value(value: Value) -> str:
    # Handle some advanced types.
    if value.type == AdvancedSequenceTypes.ARRAY:
        return f"arrayOf({convert_arguments(value.data)})"
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
    elif value.type == AdvancedNumericTypes.U_INT_32:
        return f"{value.data}U"
    elif value.type == AdvancedNumericTypes.U_INT_8:
        return f"({value.data}).toUByte()"
    elif value.type == AdvancedNumericTypes.INT_8:
        return f"({value.data}).toByte()"
    elif value.type == AdvancedNumericTypes.INT_16:
        return f"({value.data}).toShort()"
    elif value.type == AdvancedNumericTypes.U_INT_16:
        return f"({value.data}).toUShort()"
    elif value.type == AdvancedNumericTypes.INT_64:
        return f"{value.data}L"
    elif value.type == AdvancedNumericTypes.U_INT_64:
        return f"{value.data}UL"
    elif value.type == AdvancedNumericTypes.BIG_INT:
        return f'BigInteger("{value.data}")'
    elif value.type in (
        AdvancedNumericTypes.DOUBLE_EXTENDED,
        AdvancedNumericTypes.FIXED_PRECISION,
    ):
        if not isinstance(value.data, SpecialNumbers):
            return f'BigDecimal("{value.data}")'
        else:
            raise AssertionError("Special numbers not supported for BigDecimal")
    elif value.type == AdvancedStringTypes.CHAR:
        return "'" + value.data.replace("'", "\\'") + "'"
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
        return f"listOf({convert_arguments(value.data)})"
    elif value.type == BasicSequenceTypes.SET:
        return f"setOf({convert_arguments(value.data)})"
    elif value.type == BasicObjectTypes.MAP:
        result = "mapOf("
        for i, pair in enumerate(value.data):
            result += "Pair("
            result += convert_statement(pair.key)
            result += ", "
            result += convert_statement(pair.value)
            result += ")"
            if i != len(value.data) - 1:
                result += ", "
        result += ")"
        return result
    raise AssertionError(f"Invalid literal: {value!r}")


def convert_function_call(function: FunctionCall) -> str:
    result = ""
    if function.namespace and not (
        isinstance(function, PreparedFunctionCall) and function.has_root_namespace
    ):
        result += f"{convert_statement(function.namespace)}!!."
    result += function.name
    if function.type != FunctionType.PROPERTY:
        result += f"({convert_arguments(function.arguments)})"
    return result


def convert_declaration(
    tp: Union[AllTypes, VariableType, Literal["Object"]],
    value: Optional[Statement],
    nt=None,
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
        return f"{tp.data}?"
    elif tp == AdvancedSequenceTypes.ARRAY:
        # TODO: this is very complex, and why?
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, get_args(Value))
            else nt
            if nt
            else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_, False)
        return f"Array<{convert_declaration(base_type, None, sub_type)}>?"
    elif tp in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
        return "BigInteger?"
    elif tp in (
        AdvancedNumericTypes.DOUBLE_EXTENDED,
        AdvancedNumericTypes.FIXED_PRECISION,
    ):
        return "BigDecimal?"
    elif tp == AdvancedNumericTypes.INT_8:
        return "Byte?"
    elif tp in (AdvancedNumericTypes.U_INT_8, AdvancedNumericTypes.INT_16):
        return "Short?"
    elif tp in (AdvancedNumericTypes.U_INT_16, AdvancedNumericTypes.INT_32):
        return "Int?"
    elif tp in (AdvancedNumericTypes.U_INT_32, AdvancedNumericTypes.INT_64):
        return "Long?"
    elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
        return "Float?"
    elif tp == "Object":
        return "Any?"
    elif tp == AdvancedStringTypes.CHAR:
        return "Char?"
    basic = resolve_to_basic(tp)
    if basic == BasicSequenceTypes.SEQUENCE:
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, get_args(Value))
            else nt
            if nt
            else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_)
        return f"List<{convert_declaration(base_type, None, sub_type)}>?"
    elif basic == BasicSequenceTypes.SET:
        type_ = (
            (value.get_content_type() or "Object")
            if isinstance(value, get_args(Value))
            else nt
            if nt
            else "Object"
        )
        base_type, sub_type = extract_type_tuple(type_)
        return f"Set<{convert_declaration(base_type, None, sub_type)}>?"
    elif basic == BasicBooleanTypes.BOOLEAN:
        return "Boolean?"
    elif basic == BasicStringTypes.TEXT:
        return "String?"
    elif basic == BasicNumericTypes.INTEGER:
        return "Int?"
    elif basic == BasicNumericTypes.REAL:
        return "Double?"
    elif basic == BasicObjectTypes.MAP:
        if isinstance(value, get_args(Value)):
            key_type_ = value.get_key_type() or "Object"
            value_type_ = value.get_value_type() or "Object"
        elif nt:
            key_type_, value_type_ = nt
        else:
            key_type_, value_type_ = "Object", "Object"
        key_base_type, key_sub_type = extract_type_tuple(key_type_)
        value_base_type, value_sub_type = extract_type_tuple(value_type_)
        return f"Map<{convert_declaration(key_base_type, None, key_sub_type)}, {convert_declaration(value_base_type, None, value_sub_type)}>?"
    elif basic in (BasicStringTypes.ANY, BasicNothingTypes.NOTHING):
        return "Any?"
    raise AssertionError(f"Unknown type: {tp!r}")


def convert_statement(statement: Statement, full=False) -> str:
    if isinstance(statement, Identifier):
        return statement
    elif isinstance(statement, FunctionCall):
        return convert_function_call(statement)
    elif isinstance(statement, get_args(Value)):
        return convert_value(statement)
    elif isinstance(statement, get_args(Assignment)):
        prefix = "var " if full else ""
        return (
            f"{prefix}{statement.variable} = "
            f"{convert_statement(statement.expression)}"
        )
    raise AssertionError(f"Unknown statement: {statement!r}")


indent = " " * 4


def convert_execution_unit(pu: PreparedExecutionUnit) -> str:
    result = f"""
import java.io.PrintWriter
import java.math.BigInteger
import java.math.BigDecimal

class {pu.execution_name}: AutoCloseable {{

    private val valueWriter = PrintWriter("{pu.value_file}")
    private val exceptionWriter = PrintWriter("{pu.exception_file}")
    
    private fun writeSeparator() {{
        valueWriter.write("--{pu.testcase_separator_secret}-- SEP")
        exceptionWriter.write("--{pu.testcase_separator_secret}-- SEP")
        System.err.print("--{pu.testcase_separator_secret}-- SEP")
        System.out.print("--{pu.testcase_separator_secret}-- SEP")
        valueWriter.flush()
        exceptionWriter.flush()
        System.err.flush()
        System.out.flush()
    }}
    
    private fun writeContextSeparator() {{
        valueWriter.write("--{pu.context_separator_secret}-- SEP")
        exceptionWriter.write("--{pu.context_separator_secret}-- SEP")
        System.err.print("--{pu.context_separator_secret}-- SEP")
        System.out.print("--{pu.context_separator_secret}-- SEP")
        valueWriter.flush()
        exceptionWriter.flush()
        System.err.flush()
        System.out.flush()
    }}
    
    private fun sendValue(value: Any?) {{
        valuesSend(valueWriter, value)
    }}
    
    private fun sendException(throwable: Throwable?) {{
        valuesSendException(exceptionWriter, throwable)
    }}

    private fun sendSpecificValue(value: EvaluationResult) {{
        valuesSendEvaluated(valueWriter, value)
    }}

    private fun sendSpecificException(exception: EvaluationResult) {{
        valuesSendEvaluated(exceptionWriter, exception)
    }}
    """

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        result += indent + f"private fun context{i}() {{\n"
        result += indent * 2 + ctx.before + "\n"

        # Generate code for each testcase
        tc: PreparedTestcase
        for tc in ctx.testcases:
            result += indent * 2 + "this.writeSeparator()\n"

            if not tc.testcase.is_main_testcase() and isinstance(
                tc.input.statement, get_args(Assignment)
            ):
                decl = convert_declaration(
                    tc.input.statement.type, tc.input.statement.expression
                )
                result += (
                    f"{indent * 2}var {tc.input.statement.variable}: {decl} = null\n"
                )

            result += indent * 2 + "try {\n"
            if tc.testcase.is_main_testcase():
                wrapped = [json.dumps(a) for a in tc.input.arguments]
                result += indent * 3 + f"solutionMain(arrayOf({', '.join(wrapped)}))\n"
            else:
                result += (
                    indent * 3 + convert_statement(tc.input.input_statement()) + "\n"
                )
            result += indent * 3 + convert_statement(tc.exception_statement()) + "\n"
            result += indent * 2 + "} catch (e: Exception) {\n"
            result += indent * 3 + convert_statement(tc.exception_statement("e")) + "\n"
            result += indent * 2 + "} catch (e: AssertionError) {\n"
            result += indent * 3 + convert_statement(tc.exception_statement("e")) + "\n"
            result += indent * 2 + "}\n"

        result += indent * 2 + ctx.after + "\n"
        result += indent + "}\n"

    result += indent + "fun execute() {\n"
    for i, ctx in enumerate(pu.contexts):
        result += indent * 2 + "this.writeContextSeparator()\n"
        result += f"{indent * 2}this.context{i}()\n"
    result += indent + "}\n"

    result += f"""
    override fun close() {{
        this.valueWriter.close()
        this.exceptionWriter.close()
    }}
}}

fun main(args: Array<String> = emptyArray()) {{
    val execution = {pu.execution_name}()
    execution.use {{
        it.execute()
    }}
}}
"""

    return result


def convert_selector(contexts: List[str]) -> str:
    result = """
    fun main(args: Array<String>) {
        val name = args[0]
    """
    for c in contexts:
        result += f"""
            if ("{c}".equals(name)) {{
                val context = {c}()
                context.use {{
                    it.execute()
                }}
            }}
        """
    result += "}\n"
    return result


def convert_check_function(function: FunctionCall) -> str:
    return f"""
    import java.io.PrintWriter

    fun main() {{
        val result = {convert_function_call(function)}
        val writer = PrintWriter(System.out)
        valuesSendEvaluated(writer, result)
        writer.flush()
    }}
    """


def convert_encoder(values: List[Value]) -> str:
    result = """
    import java.io.PrintWriter
    import java.math.BigInteger
    import java.math.BigDecimal
    
    fun main() {
        val writer = PrintWriter(System.out)
    """

    for value in values:
        result += " " * 4 + f"valuesSend(writer, {convert_value(value)})\n"
        result += " " * 4 + 'writer.write("\\n")\n'

    result += " " * 4 + "writer.close()\n"
    result += "}\n"
    return result
