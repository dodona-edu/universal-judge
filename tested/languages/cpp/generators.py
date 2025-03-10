import json
from typing import cast

from tested.datatypes import AllTypes, resolve_to_basic
from tested.datatypes.advanced import (
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
)
from tested.datatypes.basic import (
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
from tested.languages.utils import convert_unknown_type, is_special_void_call
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
    WrappedAllTypes,
    as_basic_type,
)
from tested.testsuite import MainInput

type Subtype = WrappedAllTypes | tuple[WrappedAllTypes, WrappedAllTypes] | None


class CPPGenerator:
    def __init__(self, extension: str = "c"):
        self.extension = extension

    def convert_arguments(self, arguments: list[Expression | NamedArgument]) -> str:
        return ", ".join(
            self.convert_statement(cast(Expression, arg), True) for arg in arguments
        )

    def convert_type(self, value: Expression) -> str:
        if isinstance(value, Identifier) or isinstance(value, FunctionCall):
            return "std::any"
        else:
            return self.convert_declaration(value.type, value)

    def combine_types(self, expressions: list[Expression]) -> str:
            content_types = set(self.convert_type(content) for content in expressions)
            if len(content_types) == 1:
                return content_types.pop()
            elif len(content_types) > 1:
                return f"std::variant<{', '.join(content_types)}>"
            else:
                return "std::any"

    def sequence_subtype(self, value: Statement) -> str:
        if isinstance(value, SequenceType):
            return self.combine_types(value.data)
        else:
            return "std::any"

    def convert_value(self, value: Value, add_type = False) -> str:
        basic = as_basic_type(value)
        if add_type:
            tp = value.type
            bt = basic.type
            value_string = self.convert_value(value)
            type_string = self.convert_declaration(tp, value)

            if bt == BasicNothingTypes.NOTHING:
                return value_string

            if (tp == AdvancedNumericTypes.DOUBLE_EXTENDED
                or tp == AdvancedNumericTypes.DOUBLE_PRECISION
                or tp == AdvancedNumericTypes.SINGLE_PRECISION
                or tp == AdvancedStringTypes.CHAR
                or bt == BasicBooleanTypes.BOOLEAN
                or bt == BasicNumericTypes.REAL):
                return f"(({type_string}) {value_string})"

            return f"{type_string}({value_string})"

        if value.type == AdvancedStringTypes.CHAR:
            assert isinstance(value, StringType)
            return f"'{value.data.replace("'", "\\'")}'"
        elif value.type == AdvancedNumericTypes.INT_64:
            return f"{value.data}L"
        elif value.type == AdvancedNumericTypes.U_INT_64:
            return f"{value.data}UL"
        elif value.type == AdvancedNumericTypes.U_INT_32:
            return f"{value.data}U"
        if basic.type == BasicObjectTypes.MAP:
            return (
                "{"
                + ", ".join(
                    f"{{{self.convert_statement(kvp.key)}, {self.convert_statement(kvp.value)}}}"
                    for kvp in cast(ObjectType, basic).data
                )
                + "}"
            )
        elif (
            basic.type == BasicSequenceTypes.SEQUENCE or basic.type == BasicSequenceTypes.SET
        ):
            return (
                "{"
                + ", ".join(
                    self.convert_value(cast(Value, cast(Value, v)))
                    for v in cast(SequenceType, basic).data
                )
                + "}"
            )
        elif basic.type == BasicNumericTypes.INTEGER:
            # Basic heuristic for long numbers
            if (basic.data > (2**31 - 1)) or (basic.data < -(2**31)):
                return f"{basic.data}L"
            else:
                return str(basic.data)
        elif basic.type == BasicNumericTypes.REAL:
            suffix = "f" if value.type == AdvancedNumericTypes.SINGLE_PRECISION else ""
            if not isinstance(basic.data, SpecialNumbers):
                return str(basic.data) + suffix
            elif basic.data == SpecialNumbers.NOT_A_NUMBER:
                return "nan" + suffix + '("")'
            elif basic.data == SpecialNumbers.POS_INFINITY:
                return "INFINITY"
            else:
                assert SpecialNumbers.NEG_INFINITY
                return "(-INFINITY)"
        elif basic.type == BasicStringTypes.TEXT:
            return json.dumps(basic.data)
        elif basic.type == BasicBooleanTypes.BOOLEAN:
            return str(basic.data).lower()
        elif basic.type == BasicNothingTypes.NOTHING:
            return "nullptr"
        elif basic.type == BasicStringTypes.UNKNOWN:
            assert isinstance(basic, StringType)
            return convert_unknown_type(basic)
        raise AssertionError(f"Invalid literal: {value!r}")

    def convert_declaration(
        self,
        tp: AllTypes | VariableType,
        value: Statement,
    ) -> str:
        if isinstance(tp, VariableType):
            return tp.data
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
        elif tp == AdvancedStringTypes.STRING:
            return "std::string"
        elif tp == AdvancedNumericTypes.DOUBLE_EXTENDED:
            return "long double"
        elif tp == AdvancedNumericTypes.DOUBLE_PRECISION:
            return "double"
        elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
            return "float"
        elif tp == AdvancedStringTypes.CHAR:
            return "char"
        elif tp == AdvancedSequenceTypes.LIST:
            return f"std::list<{self.sequence_subtype(value)}>"
        elif tp == AdvancedSequenceTypes.ARRAY:
            return f"std::vector<{self.sequence_subtype(value)}>"
        elif tp == AdvancedSequenceTypes.TUPLE:
            if isinstance(value, SequenceType):
                subtype_string = ", ".join(self.convert_type(content) for content in value.data)
                return f"std::tuple<{subtype_string}>"
            else:
                return "std::tuple<std::any>"

        basic = resolve_to_basic(tp)
        if basic == BasicObjectTypes.MAP:
            if isinstance(value, ObjectType):
                key_type = self.combine_types([p.key for p in value.data])
                value_type = self.combine_types([p.value for p in value.data])
                return f"std::map<{key_type}, {value_type}>"
            else:
                return "std::map<std::any, std::any>"
        elif basic == BasicSequenceTypes.SET:
            return f"std::set<{self.sequence_subtype(value)}>"
        elif basic == BasicSequenceTypes.SEQUENCE:
            return f"std::vector<{self.sequence_subtype(value)}>"
        elif basic == BasicStringTypes.TEXT:
            return "std::string"
        elif basic == BasicStringTypes.ANY:
            return "std::any"
        elif basic == BasicNumericTypes.INTEGER:
            return "std::intmax_t"
        if basic == BasicBooleanTypes.BOOLEAN:
            return "bool"
        elif basic == BasicNumericTypes.REAL:
            return "double"
        elif basic == BasicNothingTypes.NOTHING:
            return "void"
        raise AssertionError(f"Unknown type: {tp!r}")

    def convert_statement(self, statement: Statement, add_value_type = False) -> str:
        if isinstance(statement, PropertyAssignment):
            return (
                f"{self.convert_statement(statement.property)} = "
                f"{self.convert_statement(statement.expression)}"
            )
        elif isinstance(statement, Identifier):
            return statement
        elif isinstance(statement, FunctionCall):
            return self.convert_function_call(statement)
        elif isinstance(statement, Value):
            return self.convert_value(statement, add_value_type)
        elif isinstance(statement, VariableAssignment):
            prefix = self.convert_declaration(statement.type, statement.expression)

            return (
                f"{prefix} {statement.variable} = "
                f"{self.convert_statement(statement.expression)}"
            )
        raise AssertionError(f"Unknown statement: {statement!r}")

    def is_pascal_case(self, identifier: Identifier) -> bool:
        return len(identifier) > 0 and identifier[0].isupper() and not identifier[1:].isupper()

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
            if isinstance(function.namespace, Identifier):
                if self.is_pascal_case(function.namespace):
                    # Assume namespace is a class, thus this is a static function
                    result = f"{function.namespace}::{result}"
                else:
                    result = f"{function.namespace}.{result}"
            else:
                result = "(" + self.convert_statement(function.namespace) + ")." + result
        return result

    def spacing(self, depth):
        return " " * depth * 4

    def convert_testcase(self, tc: PreparedTestcase, pu: PreparedExecutionUnit, depth = 1) -> str:
        indent = self.spacing(depth)
        result = ""
        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            wrapped = [json.dumps(a) for a in tc.input.arguments]
            result += indent + f'char* args[] = {{"{pu.submission_name}", '
            result += ", ".join(wrapped)
            result += "};\n"
            result += (
                indent
                + f"exit_code = solution_main({len(tc.input.arguments) + 1}, args);\n"
            )
        else:
            assert isinstance(tc.input, PreparedTestcaseStatement)
            result += indent + "exit_code = 0;\n"
            if is_special_void_call(tc.input, pu.language):
                # The method has a "void" return type, so don't wrap it.
                result += (
                    indent
                    + self.convert_statement(tc.input.unwrapped_input_statement())
                    + ";\n"
                )
                result += (
                    indent + self.convert_statement(tc.input.no_value_call()) + ";\n"
                )
            else:
                result += (
                    indent + self.convert_statement(tc.input.input_statement()) + ";\n"
                )
        return result

    def generate_internal_context(
        self, ctx: PreparedContext, pu: PreparedExecutionUnit
    ) -> str:
        result = ctx.before + "\n"
        result += self.spacing(1) + "int exit_code;" + "\n"

        # Generate code for each testcase
        tc: PreparedTestcase
        for i, tc in enumerate(ctx.testcases):
            result += self.spacing(i+1) + "try {" + "\n"
            result += self.spacing(i+2) + f"{pu.unit.name}_write_separator();\n"
            result += self.convert_testcase(tc, pu, i+2)

        for i in range(len(ctx.testcases), 0, -1):
            result += self.spacing(i) + "} catch (...) {\n"
            result += self.spacing(i+1) + "const std::exception_ptr &e = std::current_exception();\n"
            result += self.spacing(i+1) + self.convert_statement(
                tc.exception_statement("e")) + ";\n"
            result += self.spacing(i+1)+ "exit_code = 1;\n"
            result += self.spacing(i) + "}\n"

        result += self.spacing(1) + ctx.after + "\n"
        result += self.spacing(1) + "return exit_code;\n"
        return result

    def define_write_funtions(self, pu: PreparedExecutionUnit) -> str:
        result = f"""
static std::ofstream {pu.unit.name}_value_file;
static std::ofstream {pu.unit.name}_exception_file;

static void {pu.unit.name}_write_separator() {{
    {pu.unit.name}_value_file << "--{pu.testcase_separator_secret}-- SEP";
    {pu.unit.name}_exception_file << "--{pu.testcase_separator_secret}-- SEP";
    std::cout << "--{pu.testcase_separator_secret}-- SEP";
    std::cerr << "--{pu.testcase_separator_secret}-- SEP";
}}

static void {pu.unit.name}_write_context_separator() {{
    {pu.unit.name}_value_file << "--{pu.context_separator_secret}-- SEP";
    {pu.unit.name}_exception_file << "--{pu.context_separator_secret}-- SEP";
    std::cout << "--{pu.context_separator_secret}-- SEP";
    std::cerr << "--{pu.context_separator_secret}-- SEP";
}}

#undef send_value
#define send_value(value) write_value({pu.unit.name}_value_file, value)

#undef send_specific_value
#define send_specific_value(value) write_evaluated({pu.unit.name}_value_file, value)

#undef send_exception
#define send_exception(value) write_exception({pu.unit.name}_exception_file, value)

#undef send_specific_exception
#define send_specific_exception(value) write_evaluated({pu.unit.name}_exception_file, value)
"""
        return result

    def convert_execution_unit(self, pu: PreparedExecutionUnit) -> str:
        result = f"""
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
    {pu.unit.name}_value_file.open("{pu.value_file}", std::ios::out);
    {pu.unit.name}_exception_file.open("{pu.exception_file}", std::ios::out);
    int exit_code;
"""

        for i, ctx in enumerate(pu.contexts):
            result += self.spacing(1) + f"{pu.unit.name}_write_context_separator();\n"
            result += self.spacing(1) + f"exit_code = {pu.unit.name}_context_{i}();\n"

        result += f"""
    {pu.unit.name}_value_file.close();
    {pu.unit.name}_exception_file.close();
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
#define INCLUDED true
"""

        for ctx in contexts:
            result += f"""
#if __has_include("{ctx}.{self.extension}")
#include "{ctx}.{self.extension}"
#endif
"""

        result += """
int main(int argc, char* argv[]) {

    if (argc < 1) {
        std::cerr << "No context selected.";
        return -2;
    }

    char* name = argv[1];
"""
        for ctx in contexts:
            result += f"""
    #if __has_include("{ctx}.{self.extension}")
    if ("{ctx}" == std::string(name)) {{
        return {ctx}();
    }}
    #endif
"""

        result += """
    std::cerr << "Non-existing context '" << name << "' selected.";
    return -1;
}
"""
        return result

    def convert_encoder(self, values: list[Value]) -> str:
        result = """
#include "values.h"

int main() {
"""
        for value in values:
            result += self.spacing(1) + f"write_value(std::cout, {self.convert_value(value, True)});\n"
            result += self.spacing(1) + 'std::cout << "␞";\n'
        result += "}\n"
        return result
