import re
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Mapping, Set, Tuple

from tested.datatypes import AdvancedStringTypes, AllTypes, BasicStringTypes
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.config import CallbackResult, Command, Language
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.languages.utils import limit_output
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


class Bash(Language):
    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        return {"global_identifier": "macro_case"}

    def datatype_support(self) -> Mapping[AllTypes, TypeSupport]:
        return {
            AdvancedStringTypes.CHAR: TypeSupport.REDUCED,
            BasicStringTypes.TEXT: TypeSupport.SUPPORTED,
        }

    def file_extension(self) -> str:
        return "sh"

    def initial_dependencies(self) -> List[str]:
        return []

    def needs_selector(self):
        return False

    def supported_constructs(self) -> Set[Construct]:
        return {
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.DEFAULT_PARAMETERS,
            Construct.GLOBAL_VARIABLES,
        }

    def compilation(self, files: List[str]) -> CallbackResult:
        submission = submission_file(self)
        main_file = list(filter(lambda x: x == submission, files))
        if main_file:
            return ["bash", "-n", main_file[0]], files
        else:
            return [], files

    def compiler_output(
        self, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        regex = re.compile(f"{submission_file(self)}: " f"(regel|rule) ([0-9]+):")
        return [], [], limit_output(stdout), regex.sub("<code>:\\2:", stderr)

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        return ["bash", file, *arguments]

    def stderr(self, stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        regex = re.compile(
            f"{EXECUTION_PREFIX}_[0-9]+_[0-9]+\\."
            f"{self.file_extension()}: [a-zA-Z_]+ [0-9]+:"
        )
        script = f"./{submission_file(self)}"
        stderr = regex.sub("<testcode>:", stderr).replace(script, "<code>")
        regex = re.compile(
            f"{EXECUTION_PREFIX}_[0-9]+_[0-9]+\\." f"{self.file_extension()}"
        )
        return [], [], regex.sub("<testcode>", stderr)

    def stdout(self, stdout: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        return self.stderr(stdout)

    def linter(self, remaining: float) -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.bash import linter

        return linter.run_shellcheck(self.config.dodona, remaining)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.bash import generators

        return generators.convert_statement(statement)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.bash import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_encoder(self, values: List[Value]) -> str:
        from tested.languages.bash import generators

        return generators.convert_encoder(values)
