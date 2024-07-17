import json

from tested.languages.c.generators import convert_statement
from tested.languages.preparation import PreparedExecutionUnit, PreparedContext, \
    PreparedTestcase, PreparedTestcaseStatement
from tested.languages.utils import is_special_void_call
from tested.testsuite import MainInput


def _generate_internal_context(ctx: PreparedContext, pu: PreparedExecutionUnit) -> str:
    result = f"""
    {ctx.before}

    int exit_code;
    """

    # Generate code for each testcase
    tc: PreparedTestcase
    for tc in ctx.testcases:
        result += f"{pu.unit.name}_write_separator();\n"

        if tc.testcase.is_main_testcase():
            assert isinstance(tc.input, MainInput)
            wrapped = [json.dumps(a) for a in tc.input.arguments]
            result += f'string string_args[] = {{"{pu.submission_name}", '
            result += ", ".join(wrapped)
            result += "};\n"
            result += f"""
                // convert string to char**
                const int argc = {len(tc.input.arguments) + 1};
                char** args = new char*[argc];
                for (int i = 0; i < argc; i++) {{
                    args[i] = new char[string_args[i].size() + 1];
                    strcpy(args[i], string_args[i].c_str());
                }}
            """
            result += (
                f"exit_code = solution_main(argc, args);\n"
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
    #include "values.h"
    #include "{pu.submission_name}.cpp"
    
    using namespace std;
    """

    # Import functions
    for name in pu.evaluator_names:
        result += f'#include "{name}.cpp"\n'

    result += f"""
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
    #define send_value(value) write_value({pu.unit.name}_value_file, value)

    #undef send_specific_value
    #define send_specific_value(value) write_evaluated({pu.unit.name}_value_file, value)
    """

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        result += f"""
        int {pu.unit.name}_context_{i}(void) {{
            {_generate_internal_context(ctx, pu)}
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


def convert_selector(contexts: list[str]) -> str:
    result = """
    #include <string.h>
    #include <stdio.h>

    #define INCLUDED true
    """

    for ctx in contexts:
        result += f"""
        #if __has_include("{ctx}.cpp")
        #include "{ctx}.cpp"
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
            #if __has_include("{ctx}.cpp")
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
