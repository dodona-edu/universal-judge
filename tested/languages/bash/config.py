import re
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Tuple

from tested.dodona import AnnotateCode, Message
from tested.languages.config import CallbackResult, Command, Language, limit_output
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


class Bash(Language):
    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        return {"global_identifier": "macro_case"}

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
            f"{self.execution_prefix()}_[0-9]+_[0-9]+\\."
            f"{self.extension_file()}: [a-zA-Z_]+ [0-9]+:"
        )
        script = f"./{submission_file(self)}"
        stderr = regex.sub("<testcode>:", stderr).replace(script, "<code>")
        regex = re.compile(
            f"{self.execution_prefix()}_[0-9]+_[0-9]+\\." f"{self.extension_file()}"
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
