import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import AllTypes
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.c.generators import CGenerator
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
from tested.languages.utils import executable_name
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

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {"global_identifier": "macro_case"}

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.GLOBAL_VARIABLES,
        }

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return {  # type: ignore
            "integer": "supported",
            "real": "supported",
            "char": "supported",
            "text": "supported",
            "string": "supported",
            "boolean": "supported",
            "nothing": "supported",
            "undefined": "reduced",
            "null": "reduced",
            "int8": "reduced",
            "uint8": "reduced",
            "int16": "supported",
            "uint16": "supported",
            "int32": "supported",
            "uint32": "supported",
            "int64": "supported",
            "uint64": "supported",
            "single_precision": "supported",
            "double_precision": "supported",
            "double_extended": "supported",
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
        # We use regex to find the main function.
        # First, check if we have a no-arg main function.
        # If so, replace it with a renamed main function that does have args.
        no_args = re.compile(r"(int|void)(\s+)main(\s*)\((\s*)\)(\s*{)")
        replacement = r"int\2solution_main\3(\4int argc, char* argv[])\5"
        contents, nr = re.subn(no_args, replacement, contents, count=1)
        if nr == 0:
            # There was no main function without arguments. Now we try a main
            # function with arguments.
            with_args = re.compile(r"(int|void)(\s+)main(\s*)\((\s*)int")
            replacement = r"int\2solution_main\3(\4int"
            contents = re.sub(with_args, replacement, contents, count=1)
        with open(solution, "w") as file:
            header = "#pragma once\n\n"
            file.write(header + contents)

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.c import linter

        assert self.config
        return linter.run_cppcheck(self.config.dodona, remaining)

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        included_regex = rf"from ({EXECUTION_PREFIX}|selector)"
        result = ""
        for line in stacktrace.splitlines(keepends=True):
            if re.search(included_regex, line):
                continue

            # Once we hit the three dots, skip.
            if "..." in line:
                break

            line = line.replace(submission_file(self), "<code>")
            result += line
        return result

    def is_source_file(self, file: Path) -> bool:
        return file.suffix in (".c", ".h")

    def generator(self) -> CGenerator:
        return CGenerator(self.file_extension())

    def generate_statement(self, statement: Statement) -> str:
        return self.generator().convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        return self.generator().convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: list[str]) -> str:
        return self.generator().convert_selector(contexts)

    def generate_encoder(self, values: list[Value]) -> str:
        return self.generator().convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {  # type: ignore
                "integer": "int",
                "real": "double",
                "char": "char",
                "text": "char*",
                "string": "char*",
                "boolean": "bool",
                "nothing": "void",
                "undefined": "void",
                "int8": "signed char",
                "uint8": "unsigned char",
                "int16": "short",
                "uint16": "unsigned short",
                "int32": "int",
                "uint32": "unsigned int",
                "int64": "long",
                "uint64": "unsigned long",
                "bigint": "long long",
                "single_precision": "float",
                "double_precision": "double",
                "any": "void*",
            }
        }

    def is_void_method(self, name: str) -> bool:
        assert self.config
        regex = rf"void\s+{name}"
        the_source = self.config.dodona.source.read_text()
        return re.search(regex, the_source) is not None
