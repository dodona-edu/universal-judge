import json
from typing import List, Union, cast

from tested.datatypes import (
    AdvancedNumericTypes,
    AdvancedStringTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicStringTypes,
    resolve_to_basic,
)
from tested.languages.preparation import (
    PreparedContext,
    PreparedExecutionUnit,
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
    SpecialNumbers,
    Statement,
    StringType,
    Value,
    VariableType,
    as_basic_type,
)
from tested.testsuite import MainInput


def convert_arguments(arguments: List[Expression | NamedArgument]) -> str:
    return ", ".join(convert_statement(cast(Expression, arg)) for arg in arguments)


def convert_value(value: Value) -> str:
    # Handle some advanced types.
    if value.type == AdvancedStringTypes.CHAR:
        assert isinstance(value, StringType)
        return f"(char) '" + value.data.replace("'", "\\'") + "'"
    elif value.type == AdvancedNumericTypes.INT_16:
        return f"((short) {value.data})"
    elif value.type == AdvancedNumericTypes.U_INT_16:
        return f"((unsigned short) {value.data})"
    elif value.type == AdvancedNumericTypes.INT_64:
        return f"{value.data}L"
    elif value.type == AdvancedNumericTypes.U_INT_64:
        return f"{value.data}UL"
    elif value.type == AdvancedNumericTypes.U_INT_32:
        return f"{value.data}U"
    # Handle basic types
    original = value
    value = as_basic_type(value)
    if value.type == BasicNumericTypes.INTEGER:
        # Basic heuristic for long numbers
        if (value.data > (2**31 - 1)) or (value.data < -(2**31)):
            return f"{value.data}L"
        else:
            return str(value.data)
    elif value.type == BasicNumericTypes.REAL:
        suffix = "f" if original.type == AdvancedNumericTypes.SINGLE_PRECISION else ""
        if not isinstance(value.data, SpecialNumbers):
            return str(value.data) + suffix
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return "nan" + suffix + '("")'
        elif value.data == SpecialNumbers.POS_INFINITY:
            if original.type == AdvancedNumericTypes.DOUBLE_PRECISION:
                return "((double) INFINITY)"
            else:
                return "INFINITY"
        else:
            assert SpecialNumbers.NEG_INFINITY
            if original.type == AdvancedNumericTypes.DOUBLE_PRECISION:
                return "((double) -INFINITY)"
            else:
                return "(-INFINITY)"
    elif value.type == BasicStringTypes.TEXT:
        return json.dumps(value.data)
    elif value.type == BasicBooleanTypes.BOOLEAN:
        return f"(bool) " + str(value.data).lower()
    elif value.type == BasicNothingTypes.NOTHING:
        return "NULL"
    elif value.type == BasicStringTypes.UNKNOWN:
        assert isinstance(value, StringType)
        return convert_unknown_type(value)
    raise AssertionError(f"Invalid literal: {value!r}")


def convert_function_call(function: FunctionCall) -> str:
    result = function.name
    if function.type != FunctionType.PROPERTY:
        result += f"({convert_arguments(function.arguments)})"  # pyright: ignore
    return result


def convert_declaration(tp: Union[AllTypes, VariableType]) -> str:
    if isinstance(tp, VariableType):
        return tp.data
    elif tp == AdvancedNumericTypes.BIG_INT:
        return "long long"
    elif tp == AdvancedNumericTypes.U_INT_64:
        return "unsigned long"
    elif tp == AdvancedNumericTypes.INT_64:
        return "long"
    elif tp == AdvancedNumericTypes.U_INT_32:
        return "unsigned int"
    elif tp == AdvancedNumericTypes.INT_32:
        return "int"
    elif tp == AdvancedNumericTypes.U_INT_16:
        return "unsigned short int"
    elif tp == AdvancedNumericTypes.INT_16:
        return "short int"
    elif tp == AdvancedNumericTypes.U_INT_8:
        return "unsigned char"
    elif tp == AdvancedNumericTypes.INT_8:
        return "signed char"
    elif tp == AdvancedNumericTypes.DOUBLE_EXTENDED:
        return "long double"
    elif tp == AdvancedNumericTypes.DOUBLE_PRECISION:
        return "double"
    elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
        return "float"
    elif tp == AdvancedStringTypes.CHAR:
        return "char"
    basic = resolve_to_basic(tp)
    if basic == BasicBooleanTypes.BOOLEAN:
        return "bool"
    elif basic == BasicStringTypes.TEXT:
        return "char*"
    elif basic == BasicNumericTypes.INTEGER:
        return "long long"
    elif basic == BasicNumericTypes.REAL:
        return "double"
    elif basic == BasicNothingTypes.NOTHING:
        return "void"
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
            prefix = convert_declaration(statement.type) + " "
        else:
            prefix = ""
        return (
            f"{prefix}{statement.variable} = "
            f"{convert_statement(statement.expression)};"
        )
    raise AssertionError(f"Unknown statement: {statement!r}")


def _generate_internal_context(ctx: PreparedContext, pu: PreparedExecutionUnit) -> str:
    result = f"""
    {ctx.before}
    
    int exit_code;
    """

    # Generate code for each testcase
    tc: PreparedTestcase
    for tc in ctx.testcases:
        result += f"{pu.execution_name}_write_separator();\n"

        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            wrapped = [json.dumps(a) for a in tc.input.arguments]
            result += f'char* args[] = {{"{pu.submission_name}", '
            result += ", ".join(wrapped)
            result += "};\n"
            result += (
                f"exit_code = solution_main({len(tc.input.arguments) + 1}, args);\n"
            )
        else:
            assert isinstance(tc.input, PreparedTestcaseStatement)
            result += "exit_code = 0;\n"
            if is_special_void_call(tc.input, pu.language):
                # The method has a "void" return type, so don't wrap it.
                result += (
                    " " * 4
                    + convert_statement(tc.input.unwrapped_input_statement())
                    + ";\n"
                )
                result += " " * 4 + convert_statement(tc.input.no_value_call()) + ";\n"
            else:
                result += convert_statement(tc.input.input_statement()) + ";\n"

    result += ctx.after + "\n"
    result += "return exit_code;\n"
    return result


def convert_execution_unit(pu: PreparedExecutionUnit) -> str:
    result = f"""
    #include <stdio.h>
    #include <math.h>
    
    #include "values.h"
    #include "{pu.submission_name}.c"
    """

    # Import functions
    for name in pu.evaluator_names:
        result += f'#include "{name}.c"\n'

    result += f"""
    static FILE* {pu.execution_name}_value_file = NULL;
    static FILE* {pu.execution_name}_exception_file = NULL;
    
    static void {pu.execution_name}_write_separator() {{
        fprintf({pu.execution_name}_value_file, "--{pu.testcase_separator_secret}-- SEP");
        fprintf({pu.execution_name}_exception_file, "--{pu.testcase_separator_secret}-- SEP");
        fprintf(stdout, "--{pu.testcase_separator_secret}-- SEP");
        fprintf(stderr, "--{pu.testcase_separator_secret}-- SEP");
    }}
    
    static void {pu.execution_name}_write_context_separator() {{
        fprintf({pu.execution_name}_value_file, "--{pu.context_separator_secret}-- SEP");
        fprintf({pu.execution_name}_exception_file, "--{pu.context_separator_secret}-- SEP");
        fprintf(stdout, "--{pu.context_separator_secret}-- SEP");
        fprintf(stderr, "--{pu.context_separator_secret}-- SEP");
    }}
    
    #undef send_value
    #define send_value(value) write_value({pu.execution_name}_value_file, value)
    
    #undef send_specific_value
    #define send_specific_value(value) write_evaluated({pu.execution_name}_value_file, value)
    """

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        result += f"""
        int {pu.execution_name}_context_{i}(void) {{
            {_generate_internal_context(ctx, pu)}
        }}
        """

    result += f"""
    int {pu.execution_name}() {{
        {pu.execution_name}_value_file = fopen("{pu.value_file}", "w");
        {pu.execution_name}_exception_file = fopen("{pu.exception_file}", "w");
        int exit_code;
    """

    for i, ctx in enumerate(pu.contexts):
        result += " " * 4 + f"{pu.execution_name}_write_context_separator();\n"
        result += " " * 4 + f"exit_code = {pu.execution_name}_context_{i}();\n"

    result += f"""
        fclose({pu.execution_name}_value_file);
        fclose({pu.execution_name}_exception_file);
        return exit_code;
    }}
    
    #ifndef INCLUDED
    int main() {{
        return {pu.execution_name}();
    }}
    #endif
    """
    return result


def convert_selector(contexts: List[str]) -> str:
    result = """
    #include <string.h>
    #include <stdio.h>
    
    #define INCLUDED true
    """

    for ctx in contexts:
        result += f'#include "{ctx}.c"\n'

    result += """
    int main(int argc, const char* argv[]) {

        if (argc < 1) {
            fprintf(stderr, "No context selected.");
            return -2;
        }
    
        const char* name = argv[1];
    """
    for ctx in contexts:
        result += f"""
            if (strcmp("{ctx}", name) == 0) {{
                return {ctx}();
            }}
        """

    result += """
        fprintf(stderr, "Non-existing context '%s' selected.", name);
        return -1;
    }
    """
    return result


def convert_encoder(values: List[Value]) -> str:
    result = """
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "values.h"

int main() {
"""
    for value in values:
        result += " " * 4 + f"write_value(stdout, {convert_value(value)});\n"
        result += " " * 4 + 'printf("\\n");\n'
    result += "}\n"
    return result
