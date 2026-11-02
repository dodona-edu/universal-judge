import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
    AdvancedStringTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicStringTypes,
)
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.languages.language import (
    CallbackResult,
    Command,
    Language,
    TypeDeclarationMetadata,
)
from tested.languages.utils import executable_name, shift_gcc_gutter_line_number
from tested.serialisation import Statement, Value

logger = logging.getLogger(__name__)


if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


class C(Language):
    def initial_dependencies(self) -> list[str]:
        return ["values.h", "values.c", "evaluation_result.h", "evaluation_result.c"]

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "c"

    def comment(self, text: str) -> str:
        return f"// {text}"

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {"global_identifier": "macro_case"}

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.GLOBAL_VARIABLES,
        }

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return {
            BasicNumericTypes.INTEGER: TypeSupport.SUPPORTED,
            BasicNumericTypes.REAL: TypeSupport.SUPPORTED,
            AdvancedStringTypes.CHAR: TypeSupport.SUPPORTED,
            BasicStringTypes.TEXT: TypeSupport.SUPPORTED,
            AdvancedStringTypes.STRING: TypeSupport.SUPPORTED,
            BasicBooleanTypes.BOOLEAN: TypeSupport.SUPPORTED,
            BasicNothingTypes.NOTHING: TypeSupport.SUPPORTED,
            AdvancedNothingTypes.UNDEFINED: TypeSupport.REDUCED,
            AdvancedNothingTypes.NULL: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_8: TypeSupport.REDUCED,
            AdvancedNumericTypes.U_INT_8: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_16: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_16: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.INT_32: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_32: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.INT_64: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_64: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.SINGLE_PRECISION: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.DOUBLE_PRECISION: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.DOUBLE_EXTENDED: TypeSupport.SUPPORTED,
        }

    def compilation(self, files: list[str]) -> CallbackResult:
        main_file = files[-1]
        exec_file = Path(main_file).stem
        result = executable_name(exec_file)
        assert self.config
        return (
            [
                "gcc",
                "-std=c11",
                "-Wall",
                "-O3" if self.config.options.compiler_optimizations else "-O0",
                "evaluation_result.c",
                "values.c",
                main_file,
                "-Wl,--wrap=main",
                "-o",
                result,
            ],
            [result],
        )

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        local_file = cwd / executable_name(Path(file).stem)
        return [str(local_file.absolute()), *arguments]

    def modify_solution(self, solution: Path):
        with open(solution, "r") as file:
            contents = file.read()
        # The student's main stays main, so the compiler adds the implicit
        # return 0; linking with --wrap=main makes the generated __wrap_main
        # the entry point and lets it call the student's main as __real_main.
        # Only a void main must become int, as C++ rejects it.
        contents = re.sub(r"\bvoid(\s+)main(\s*)\(", r"int\1main\2(", contents, count=1)
        assert self.config
        with open(solution, "w") as file:
            header = "#pragma once\n\n"
            file.write(header + contents)
        self.config.dodona.source_offset -= header.count("\n")

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.c import linter

        assert self.config
        return linter.run_cppcheck(self.config.dodona, remaining)

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        assert self.config
        included_regex = rf"from ({EXECUTION_PREFIX}|selector)"
        result = ""
        in_student_file = False
        for line in stacktrace.splitlines(keepends=True):
            if re.search(included_regex, line):
                continue

            # Once we hit the three dots, skip.
            if line.strip() == "...":
                break

            line = line.replace(submission_file(self), "<code>")
            if re.match(r" *(\d+ )?\|", line):
                if in_student_file:
                    line = shift_gcc_gutter_line_number(
                        self.config.dodona.source_offset, line
                    )
            else:
                in_student_file = "<code>" in line
            result += line
        return result

    def is_source_file(self, file: Path) -> bool:
        return file.suffix in (".c", ".h")

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.c import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.c import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: list[str]) -> str:
        from tested.languages.c import generators

        return generators.convert_selector(contexts)

    def generate_encoder(self, values: list[Value]) -> str:
        from tested.languages.c import generators

        return generators.convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {
                BasicNumericTypes.INTEGER: "int",
                BasicNumericTypes.REAL: "double",
                AdvancedStringTypes.CHAR: "char",
                BasicStringTypes.TEXT: "char*",
                AdvancedStringTypes.STRING: "char*",
                BasicBooleanTypes.BOOLEAN: "bool",
                BasicNothingTypes.NOTHING: "void",
                AdvancedNothingTypes.UNDEFINED: "void",
                AdvancedNumericTypes.INT_8: "signed char",
                AdvancedNumericTypes.U_INT_8: "unsigned char",
                AdvancedNumericTypes.INT_16: "short",
                AdvancedNumericTypes.U_INT_16: "unsigned short",
                AdvancedNumericTypes.INT_32: "int",
                AdvancedNumericTypes.U_INT_32: "unsigned int",
                AdvancedNumericTypes.INT_64: "long",
                AdvancedNumericTypes.U_INT_64: "unsigned long",
                AdvancedNumericTypes.BIG_INT: "long long",
                AdvancedNumericTypes.SINGLE_PRECISION: "float",
                AdvancedNumericTypes.DOUBLE_PRECISION: "double",
                BasicStringTypes.ANY: "void*",
            }
        }

    def is_void_method(self, name: str) -> bool:
        assert self.config
        regex = rf"void\s+{re.escape(name)}(?!\w)"
        the_source = self.config.dodona.source.read_text()
        return re.search(regex, the_source) is not None
