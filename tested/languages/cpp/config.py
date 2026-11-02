import re
from pathlib import Path

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
    AdvancedObjectTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.languages.cpp.generators import CPPGenerator
from tested.languages.language import (
    CallbackResult,
    Command,
    Language,
    TypeDeclarationMetadata,
)
from tested.languages.preparation import PreparedExecutionUnit
from tested.languages.utils import executable_name, shift_gcc_gutter_line_number
from tested.serialisation import Statement, Value


class CPP(Language):
    def initial_dependencies(self) -> list[str]:
        return [
            "values.h",
            "values.cpp",
            "values.tpp",
            "evaluation_result.h",
            "evaluation_result.cpp",
        ]

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "cpp"

    def comment(self, text: str) -> str:
        return f"// {text}"

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {
            "identifier": "camel_case",
            "property": "camel_case",
            "class": "pascal_case",
            "global_identifier": "macro_case",
        }

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.GLOBAL_VARIABLES,
            Construct.OBJECTS,
            Construct.HETEROGENEOUS_COLLECTIONS,
            Construct.DEFAULT_PARAMETERS,
            Construct.HETEROGENEOUS_ARGUMENTS,
            Construct.EXCEPTIONS,
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
            AdvancedNumericTypes.INT_8: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_8: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.INT_16: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_16: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.INT_32: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_32: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.INT_64: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_64: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.SINGLE_PRECISION: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.DOUBLE_PRECISION: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.DOUBLE_EXTENDED: TypeSupport.SUPPORTED,
            BasicSequenceTypes.SEQUENCE: TypeSupport.SUPPORTED,
            BasicSequenceTypes.SET: TypeSupport.SUPPORTED,
            BasicObjectTypes.MAP: TypeSupport.SUPPORTED,
            AdvancedObjectTypes.DICTIONARY: TypeSupport.SUPPORTED,
            AdvancedObjectTypes.OBJECT: TypeSupport.REDUCED,
            AdvancedSequenceTypes.ARRAY: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.LIST: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.TUPLE: TypeSupport.SUPPORTED,
        }

    def compilation(self, files: list[str]) -> CallbackResult:
        main_file = files[-1]
        exec_file = Path(main_file).stem
        result = executable_name(exec_file)
        assert self.config
        return (
            [
                "g++",
                "-std=c++20",
                "-Wall",
                "-O3" if self.config.options.compiler_optimizations else "-O0",
                "evaluation_result.cpp",
                "values.cpp",
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
        return linter.run_cppcheck(self.config.dodona, remaining, "c++")

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        assert self.config
        result = ""
        in_student_file = False
        for line in stacktrace.splitlines(keepends=True):
            if submission_file(self) in line:
                in_student_file = True
            elif "|" not in line:
                in_student_file = False

            if in_student_file:
                line = line.replace(submission_file(self), "<code>")
                line = shift_gcc_gutter_line_number(
                    self.config.dodona.source_offset, line
                )
                result += line
        # Errors outside the submission (e.g. a missing main) never mention it.
        return result or stacktrace

    def is_source_file(self, file: Path) -> bool:
        return file.suffix in (".cpp", ".h", ".tpp")

    def generator(self) -> CPPGenerator:
        return CPPGenerator(self.file_extension())

    def generate_statement(self, statement: Statement) -> str:
        return self.generator().convert_statement(statement)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        return self.generator().convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: list[str]) -> str:
        return self.generator().convert_selector(contexts)

    def generate_encoder(self, values: list[Value]) -> str:
        return self.generator().convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {
                BasicNumericTypes.INTEGER: "int",
                BasicNumericTypes.REAL: "double",
                AdvancedStringTypes.CHAR: "char",
                BasicStringTypes.TEXT: "std::string",
                AdvancedStringTypes.STRING: "std::string",
                BasicBooleanTypes.BOOLEAN: "bool",
                BasicNothingTypes.NOTHING: "void",
                AdvancedNothingTypes.UNDEFINED: "void",
                AdvancedNumericTypes.INT_8: "std::int8_t",
                AdvancedNumericTypes.U_INT_8: "std::uint8_t",
                AdvancedNumericTypes.INT_16: "std::int16_t",
                AdvancedNumericTypes.U_INT_16: "std::uint16_t",
                AdvancedNumericTypes.INT_32: "std::int32_t",
                AdvancedNumericTypes.U_INT_32: "std::uint32_t",
                AdvancedNumericTypes.INT_64: "std::int64_t",
                AdvancedNumericTypes.U_INT_64: "std::uint64_t",
                AdvancedNumericTypes.BIG_INT: "std::intmax_t",
                AdvancedNumericTypes.SINGLE_PRECISION: "float",
                AdvancedNumericTypes.DOUBLE_PRECISION: "double",
                BasicStringTypes.ANY: "std::any",
                BasicSequenceTypes.SEQUENCE: "std::vector",
                AdvancedSequenceTypes.ARRAY: "std::vector",
                AdvancedSequenceTypes.LIST: "std::list",
                AdvancedSequenceTypes.TUPLE: "std::tuple",
                BasicSequenceTypes.SET: "std::set",
                BasicObjectTypes.MAP: "std::map",
            },
            "nested": ("<", ">"),
            "exception": "std::exception_ptr",
        }

    def is_void_method(self, name: str) -> bool:
        assert self.config
        regex = rf"void\s+{re.escape(name)}(?!\w)"
        the_source = self.config.dodona.source.read_text()
        return re.search(regex, the_source) is not None
