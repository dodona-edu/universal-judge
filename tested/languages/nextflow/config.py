import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import AdvancedStringTypes, AllTypes, BasicNumericTypes, BasicStringTypes
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.conventionalize import Conventionable, NamingConventions
from tested.languages.language import (
    CallbackResult,
    Command,
    Language,
    TypeDeclarationMetadata,
)
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit

class Nextflow(Language):
    def compilation(self, files: list[str]) -> CallbackResult:
        return ["groovyc", *files], files

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        named_arguments = [f"--p{i}={x}" for i, x in enumerate(arguments)]
        return ["nextflow", "-quiet", "run", file, *named_arguments]

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {
            "function": "camel_case",
        }

    def file_extension(self) -> str:
        return "nf"

    def initial_dependencies(self) -> list[str]:
        return []

    def needs_selector(self):
        return False

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.ASSIGNMENTS,
            Construct.FUNCTION_CALLS,
            Construct.HETEROGENEOUS_ARGUMENTS,
            Construct.NAMED_ARGUMENTS
        }

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return {
            AdvancedStringTypes.CHAR: TypeSupport.REDUCED,
            AdvancedStringTypes.STRING: TypeSupport.SUPPORTED,
            BasicStringTypes.TEXT: TypeSupport.SUPPORTED,
            BasicNumericTypes.INTEGER: TypeSupport.SUPPORTED,
        }

    def modify_solution(self, solution: Path):
        with open(solution, "r") as file:
            contents = file.read()
        # We use regex to find the workflow.
        # First, check if we have an unnamed workflow.
        # If so, replace it with a named workflow.
        no_name = re.compile(r"workflow(\s*{)")
        replacement = r"workflow solution_main\1"
        contents, nr = re.subn(no_name, replacement, contents, count=1)
        if nr == 0:
            # There was no unnamed workflow.
            # Now we try a named workflow.
            with_name = re.compile(r"workflow(\s+)(\w+)(\s*{)")
            replacement = r"workflow\1solution_main\3"
            contents = re.sub(with_name, replacement, contents, count=1)
        with open(solution, "w") as file:
            file.write(contents)

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.nextflow import linter

        assert self.config
        return linter.run_codenarc(self.config.dodona, remaining)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.nextflow import generators

        return generators.convert_statement(statement)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.nextflow import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_encoder(self, values: list[Value]) -> str:
        raise NotImplementedError

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        raise NotImplementedError
