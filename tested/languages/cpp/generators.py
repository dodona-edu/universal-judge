import json
from typing import cast

from tested.datatypes import AllTypes, resolve_to_basic
from tested.datatypes.advanced import (
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
)
from tested.datatypes.basic import (
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes, BasicBooleanTypes,
)
from tested.languages.preparation import (
    PreparedExecutionUnit,
    PreparedFunctionCall,
    PreparedTestcase,
    PreparedTestcaseStatement, PreparedContext,
)
from tested.languages.utils import convert_unknown_type, is_special_void_call
from tested.serialisation import (
    FunctionCall,
    FunctionType,
    ObjectType,
    PropertyAssignment,
    SequenceType,
    Statement,
    Value,
    VariableAssignment,
    VariableType,
    WrappedAllTypes, Expression, NamedArgument, SpecialNumbers, StringType,
    as_basic_type, Identifier,
)
from tested.testsuite import MainInput

type Subtype = WrappedAllTypes | tuple[WrappedAllTypes, WrappedAllTypes] | None


class CPPGenerator:
    def __init__(self, extension: str = "c"):
        self.extension = extension

    def convert_arguments(self, arguments: list[Expression | NamedArgument]) -> str:
        return ", ".join(
            self.convert_statement(cast(Expression, arg)) for arg in arguments
        )

    def unpack_wrapped_types(
        self, type_or_types: WrappedAllTypes
    ) -> tuple[AllTypes, Subtype]:
        if isinstance(type_or_types, tuple):
            return type_or_types
        return type_or_types, None

    def convert_sequence_subtype(
        self, value: Statement | None, subtype: Subtype
    ) -> str | None:
        if value and isinstance(value, SequenceType):
            # if the value is a sequence, we need to know the types of it's elements
            type_or_types = value.get_content_type()
        elif subtype:
            # we might already have a subtype extracted from a previous recursive call
            type_or_types = cast(WrappedAllTypes, subtype)
        else:
            # c++ has no default type such as Object in java, so we can't infer the type
            return None

        tp, new_subtype = self.unpack_wrapped_types(type_or_types)
        return self.convert_declaration(tp, None, new_subtype)

    def convert_map_subtypes(
        self, value: Statement | None, subtype: Subtype
    ) -> tuple[str, str] | None:
        if value and isinstance(value, ObjectType):
            key_type = value.get_key_type()
            value_type = value.get_value_type()
        elif subtype:
            key_type, value_type = cast(
                tuple[WrappedAllTypes, WrappedAllTypes], subtype
            )
        else:
            return None
        key_base_type, key_sub_type = self.unpack_wrapped_types(key_type)
        value_base_type, value_sub_type = self.unpack_wrapped_types(value_type)
        key_type_str = self.convert_declaration(key_base_type, None, key_sub_type)
        value_type_str = self.convert_declaration(value_base_type, None, value_sub_type)

        return key_type_str, value_type_str

    def convert_value(self, value: Value) -> str:
        tp = value.type
        basic = resolve_to_basic(tp)
        if basic == BasicObjectTypes.MAP:
            return (
                "{"
                + ", ".join(
                    f"{self.convert_value(cast(Value, kvp.key)), self.convert_value(cast(Value, kvp.value))}"
                    for kvp in cast(ObjectType, value).data
                )
                + "}"
            )
        elif basic == BasicSequenceTypes.SEQUENCE or basic == BasicSequenceTypes.SET:
            return (
                "{"
                + ", ".join(
                    self.convert_value(cast(Value, v))
                    for v in cast(SequenceType, value).data
                )
                + "}"
            )
        elif value.type == BasicNothingTypes.NOTHING:
            return ""

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
            if (value.data > (2 ** 31 - 1)) or (value.data < -(2 ** 31)):
                return f"{value.data}L"
            else:
                return str(value.data)
        elif value.type == BasicNumericTypes.REAL:
            suffix = (
                "f" if original.type == AdvancedNumericTypes.SINGLE_PRECISION else ""
            )
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

    def convert_declaration(
        self,
        tp: AllTypes | VariableType,
        value: Statement | None = None,
        subtype: Subtype = None,
    ) -> str:
        if isinstance(tp, VariableType):
            return tp.data + "*"
        elif tp == AdvancedNumericTypes.BIG_INT:
            return "std::intmax_t"
        elif tp == AdvancedNumericTypes.U_INT_64:
            return "std::uint64_t"
        elif tp == AdvancedNumericTypes.INT_64:
            return "std::int64_t"
        elif tp == AdvancedNumericTypes.U_INT_32:
            return "std::uint32_t"
        elif tp == AdvancedNumericTypes.INT_32:
            return "std::int32_t"
        elif tp == AdvancedNumericTypes.U_INT_16:
            return "std::uint16_t"
        elif tp == AdvancedNumericTypes.INT_16:
            return "std::int16_t"
        elif tp == AdvancedNumericTypes.U_INT_8:
            return "std::uint8_t"
        elif tp == AdvancedNumericTypes.INT_8:
            return "std::int8_t"
        if tp == AdvancedSequenceTypes.LIST:
            subtype_string = self.convert_sequence_subtype(value, subtype)
            return f"std::list<{subtype_string}>"
        elif tp == AdvancedSequenceTypes.TUPLE:
            # this method does not support tuples within sequences such as list<tuple<int, int>>
            # as value won't be defined in that case and we cant't infer the tuple's length
            # we also don't support tuples with different types, as we can only extract one type
            assert value is not None and isinstance(value, SequenceType)
            tuple_length = len(value.data)
            subtype_string = self.convert_sequence_subtype(value, subtype)
            assert subtype_string is not None
            return (
                f"std::tuple<{", ".join(subtype_string for _ in range(tuple_length))}>"
            )
        elif tp == AdvancedSequenceTypes.ARRAY:
            subtype_string = self.convert_sequence_subtype(value, subtype)
            return f"std::vector<{subtype_string}>"
        elif tp == AdvancedStringTypes.STRING:
            return "std::string"

        basic = resolve_to_basic(tp)
        if basic == BasicObjectTypes.MAP:
            subtype_strings = self.convert_map_subtypes(value, subtype)
            if subtype_strings is None:
                return "std::map<>"
            key_type, value_type = subtype_strings
            return f"std::map<{key_type}, {value_type}>"
        elif basic == BasicSequenceTypes.SET:
            subtype_string = self.convert_sequence_subtype(value, subtype)
            return f"std::set<{subtype_string}>"
        elif basic == BasicSequenceTypes.SEQUENCE:
            subtype_string = self.convert_sequence_subtype(value, subtype)
            return f"std::vector<{subtype_string}>"
        elif basic == BasicStringTypes.TEXT:
            return "std::string"
        elif basic == BasicStringTypes.ANY:
            return "std::any"
        elif basic == BasicNumericTypes.INTEGER:
            return "std::intmax_t"

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

    def convert_statement(self, statement: Statement, full=False) -> str:
        # support for property assignments
        if isinstance(statement, PropertyAssignment):
            return (
                f"{self.convert_statement(statement.property)} = "
                f"{self.convert_statement(statement.expression)};"
            )
        # overwrite the default implementation for variable assignments to allow for
        # object declarations
        elif full and isinstance(statement, VariableAssignment):

            prefix = self.convert_declaration(statement.type, statement.expression)
            return (
                f"{prefix} {statement.variable} = "
                f"{self.convert_statement(statement.expression)}"
            )

        if isinstance(statement, Identifier):
            return statement
        elif isinstance(statement, FunctionCall):
            return self.convert_function_call(statement)
        elif isinstance(statement, Value):
            return self.convert_value(statement)
        elif isinstance(statement, VariableAssignment):
            if full:
                prefix = self.convert_declaration(statement.type) + " "
            else:
                prefix = ""
            return (
                f"{prefix}{statement.variable} = "
                f"{self.convert_statement(statement.expression)};"
            )
        raise AssertionError(f"Unknown statement: {statement!r}")

    def convert_function_call(self, function: FunctionCall) -> str:
        result = function.name
        if function.type != FunctionType.PROPERTY:
            result += (
                f"({self.convert_arguments(function.arguments)})"  # pyright: ignore
            )

        # if the function has a namespace, that is not the root namespace we assume it is a method call
        if (
            function.namespace
            and not (
                isinstance(function, PreparedFunctionCall)
                and function.has_root_namespace
            )
            and not function.type == FunctionType.CONSTRUCTOR
        ):
            result = self.convert_statement(function.namespace) + "->" + result
        # add the new keyword to constructors
        if function.type == FunctionType.CONSTRUCTOR:
            result = "new " + result
        return result

    def convert_testcase(self, tc: PreparedTestcase, pu: PreparedExecutionUnit) -> str:
        result = ""
        # Define variables before asignment outside the try block
        if (
            not tc.testcase.is_main_testcase()
            and isinstance(tc.input, PreparedTestcaseStatement)
            and isinstance(tc.input.statement, VariableAssignment)
        ):
            prefix = self.convert_declaration(
                tc.input.statement.type, tc.input.statement.expression
            )
            result += f"{prefix} {tc.input.statement.variable};\n"

        # catch exceptions and write them to the output
        result += "try {" + "\n"
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
                        + self.convert_statement(tc.input.unwrapped_input_statement())
                        + ";\n"
                )
                result += (
                        " " * 4 + self.convert_statement(
                    tc.input.no_value_call()) + ";\n"
                )
            else:
                result += self.convert_statement(tc.input.input_statement()) + ";\n"
        result += "\n} catch (std::exception_ptr e) {\n"
        result += self.convert_statement(tc.exception_statement("e")) + ";\n"
        result += "exit_code = 1;\n"
        result += "}\n"
        return result

    def generate_internal_context(
            self, ctx: PreparedContext, pu: PreparedExecutionUnit
    ) -> str:
        result = f"""
        {ctx.before}

        int exit_code;
        """

        # Generate code for each testcase
        tc: PreparedTestcase
        for tc in ctx.testcases:
            result += f"{pu.unit.name}_write_separator();\n"
            result += self.convert_testcase(tc, pu)

        result += ctx.after + "\n"
        result += "return exit_code;\n"
        return result

    def define_write_funtions(self, pu: PreparedExecutionUnit) -> str:
        result = f"""
        static FILE* {pu.unit.name}_value_file = NULL;
        static FILE* {pu.unit.name}_exception_file = NULL;
        
        static void {pu.unit.name}_write_separator() {{
            fprintf({pu.unit.name}_value_file, "--{pu.testcase_separator_secret}-- SEP");
            fprintf({pu.unit.name}_exception_file, "--{pu.testcase_separator_secret}-- SEP");
            fprintf(stdout, "--{pu.testcase_separator_secret}-- SEP");
            fprintf(stderr, "--{pu.testcase_separator_secret}-- SEP");
        }}
        
        static void {pu.unit.name}_write_context_separator() {{
            fprintf({pu.unit.name}_value_file, "--{pu.context_separator_secret}-- SEP");
            fprintf({pu.unit.name}_exception_file, "--{pu.context_separator_secret}-- SEP");
            fprintf(stdout, "--{pu.context_separator_secret}-- SEP");
            fprintf(stderr, "--{pu.context_separator_secret}-- SEP");
        }}
        
        #undef send_value
        #define send_value(...) write_value({pu.unit.name}_value_file __VA_OPT__(,) __VA_ARGS__)
        
        #undef send_specific_value
        #define send_specific_value(value) write_evaluated({pu.unit.name}_value_file, value)
        """

        # add a write function for exceptions
        result += f"""
        #undef send_exception
        #define send_exception(value) write_exception({pu.unit.name}_value_file, value)
        """
        return result

    def convert_execution_unit(self, pu: PreparedExecutionUnit) -> str:
        result = f"""
        #include <stdio.h>
        #include <math.h>

        #include "values.h"
        #include "{pu.submission_name}.{self.extension}"
        """

        # Import functions
        for name in pu.evaluator_names:
            result += f'#include "{name}.{self.extension}"\n'

        result += self.define_write_funtions(pu)

        # Generate code for each context.
        ctx: PreparedContext
        for i, ctx in enumerate(pu.contexts):
            result += f"""
            int {pu.unit.name}_context_{i}(void) {{
                {self.generate_internal_context(ctx, pu)}
            }}
            """

        result += f"""
        int {pu.unit.name}() {{
            {pu.unit.name}_value_file = fopen("{pu.value_file}", "w");
            {pu.unit.name}_exception_file = fopen("{pu.exception_file}", "w");
            int exit_code;
        """

        for i, ctx in enumerate(pu.contexts):
            result += " " * 4 + f"{pu.unit.name}_write_context_separator();\n"
            result += " " * 4 + f"exit_code = {pu.unit.name}_context_{i}();\n"

        result += f"""
            fclose({pu.unit.name}_value_file);
            fclose({pu.unit.name}_exception_file);
            return exit_code;
        }}

        #ifndef INCLUDED
        int main() {{
            return {pu.unit.name}();
        }}
        #endif
        """
        return result

    def convert_selector(self, contexts: list[str]) -> str:
        result = """
        #include <string.h>
        #include <stdio.h>

        #define INCLUDED true
        """

        for ctx in contexts:
            result += f"""
            #if __has_include("{ctx}.{self.extension}")
            #include "{ctx}.{self.extension}"
            #endif
            """

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
                #if __has_include("{ctx}.{self.extension}")
                if (strcmp("{ctx}", name) == 0) {{
                    return {ctx}();
                }}
                #endif
            """

        result += """
            fprintf(stderr, "Non-existing context '%s' selected.", name);
            return -1;
        }
        """
        return result

    def convert_encoder(self, values: list[Value]) -> str:
        result = """
    #include <stdio.h>
    #include <stdlib.h>
    #include <math.h>

    #include "values.h"

    int main() {
    """
        for value in values:
            result += " " * 4 + f"write_value(stdout, {self.convert_value(value)});\n"
            result += " " * 4 + 'printf("␞");\n'
        result += "}\n"
        return result
