from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Tuple

from tested.dodona import AnnotateCode, Message
from tested.languages.config import CallbackResult, Command, Language, executable_name
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.languages.utils import (
    cleanup_description,
    haskell_cleanup_stacktrace,
    haskell_solution,
)
from tested.serialisation import FunctionCall, Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


class Haskell(Language):
    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        return {
            "namespace": "pascal_case",
            "identifier": "camel_case",
            "global_identifier": "camel_case",
            "function": "camel_case",
        }

    def compilation(self, files: List[str]) -> CallbackResult:
        main_ = files[-1]
        exec_ = main_.rstrip(".hs")
        return [
            "ghc",
            "-fno-cse",
            "-fno-full-laziness",
            "-O3" if self.config.options.compiler_optimizations else "-O0",
            main_,
            "-main-is",
            exec_,
        ], [executable_name(exec_)]

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        local_file = cwd / file
        return [str(local_file.absolute()), *arguments]

    def modify_solution(self, solution: Path):
        haskell_solution(self, solution)

    def linter(self, remaining: float) -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.haskell import linter

        return linter.run_hlint(self.config.dodona, remaining)

    def cleanup_description(self, namespace: str, description: str) -> str:
        return cleanup_description(self, namespace, description)

    def cleanup_stacktrace(
        self, traceback: str, submission_file: str, reduce_all=False
    ) -> str:
        return haskell_cleanup_stacktrace(traceback, submission_file, reduce_all)

    def compiler_output(
        self, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        return (
            [],
            [],
            "",
            haskell_cleanup_stacktrace(stderr, submission_file(self)),
        )

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.haskell import generators

        return generators.convert_statement(statement)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.haskell import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: List[str]) -> str:
        from tested.languages.haskell import generators

        return generators.convert_selector(contexts)

    def generate_check_function(self, name: str, function: FunctionCall) -> str:
        from tested.languages.haskell import generators

        return generators.convert_check_function(name, function)

    def generate_encoder(self, values: List[Value]) -> str:
        from tested.languages.haskell import generators

        return generators.convert_encoder(values)
