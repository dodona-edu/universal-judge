import re
import typing
from pathlib import Path
from typing import List, Tuple

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Message
from tested.languages.config import (
    CallbackResult,
    Command,
    Config,
    Language,
    limit_output,
)
from tested.serialisation import Statement, Value

if typing.TYPE_CHECKING:
    from tested.languages.generator import PreparedExecutionUnit


class Bash(Language):
    def compilation(self, bundle: Bundle, files: List[str]) -> CallbackResult:
        submission_file = self.with_extension(
            self.conventionalize_namespace(self.submission_name(bundle.suite))
        )
        main_file = list(filter(lambda x: x == submission_file, files))
        if main_file:
            return ["bash", "-n", main_file[0]], files
        else:
            return [], files

    def compiler_output(
        self, namespace: str, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        regex = re.compile(
            f"{self.with_extension(self.conventionalize_namespace(namespace))}: "
            f"(regel|rule) ([0-9]+):"
        )
        return [], [], limit_output(stdout), regex.sub("<code>:\\2:", stderr)

    def execution(
        self, config: Config, cwd: Path, file: str, arguments: List[str]
    ) -> Command:
        return ["bash", file, *arguments]

    def stderr(
        self, bundle: Bundle, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str]:
        regex = re.compile(
            f"{self.execution_prefix()}_[0-9]+_[0-9]+\\."
            f"{self.extension_file()}: [a-zA-Z_]+ [0-9]+:"
        )
        script = f"./{self.with_extension(self.submission_name(bundle.suite))}"
        stderr = regex.sub("<testcode>:", stderr).replace(script, "<code>")
        regex = re.compile(
            f"{self.execution_prefix()}_[0-9]+_[0-9]+\\." f"{self.extension_file()}"
        )
        return [], [], regex.sub("<testcode>", stderr)

    def stdout(
        self, bundle: Bundle, stdout: str
    ) -> Tuple[List[Message], List[AnnotateCode], str]:
        return self.stderr(bundle, stdout)

    def linter(
        self, bundle: Bundle, submission: Path, remaining: float
    ) -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.bash import linter

        return linter.run_shellcheck(bundle, submission, remaining)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.bash import generators

        return generators.convert_statement(statement)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.bash import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_encoder(self, values: List[Value]) -> str:
        from tested.languages.bash import generators

        return generators.convert_encoder(values)
