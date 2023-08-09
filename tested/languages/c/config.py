import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Mapping, Set, Tuple

from tested.datatypes import AllTypes
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.config import CallbackResult, Command, Language
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.languages.utils import executable_name
from tested.serialisation import Statement, Value

logger = logging.getLogger(__name__)


if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


class C(Language):
    def initial_dependencies(self) -> List[str]:
        return ["values.h", "values.c", "evaluation_result.h", "evaluation_result.c"]

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "c"

    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        return {"global_identifier": "macro_case"}

    def supported_constructs(self) -> Set[Construct]:
        return {
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.GLOBAL_VARIABLES,
        }

    def datatype_support(self) -> Mapping[AllTypes, TypeSupport]:
        return {  # type: ignore
            "integer": "supported",
            "real": "supported",
            "char": "supported",
            "text": "supported",
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

    def compilation(self, files: List[str]) -> CallbackResult:
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

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        local_file = cwd / executable_name(Path(file).stem)
        return [str(local_file.absolute()), *arguments]

    def modify_solution(self, solution: Path):
        with open(solution, "r") as file:
            contents = file.read()
        # We use regex to find the main function.
        # First, check if we have a no-arg main function.
        # If so, replace it with a renamed main function that does have args.
        no_args = re.compile(r"(int|void)(\s+)main(\s*)\((\s*)\)(\s*{)")
        replacement = r"int\2solution_main\3(\4int argc, char** argv)\5"
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

    def linter(self, remaining: float) -> Tuple[List[Message], List[AnnotateCode]]:
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

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.c import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.c import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: List[str]) -> str:
        from tested.languages.c import generators

        return generators.convert_selector(contexts)

    def generate_encoder(self, values: List[Value]) -> str:
        from tested.languages.c import generators

        return generators.convert_encoder(values)
