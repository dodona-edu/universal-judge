import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import AdvancedStringTypes, AllTypes, BasicStringTypes
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.config import (
    CallbackResult,
    Command,
    Language,
    TypeDeclarationMetadata,
)
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    Conventionable,
    NamingConventions,
    submission_file,
    submission_name,
)
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


class Bash(Language):
    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {"global_identifier": "macro_case"}

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return {
            AdvancedStringTypes.CHAR: TypeSupport.REDUCED,
            AdvancedStringTypes.STRING: TypeSupport.SUPPORTED,
            BasicStringTypes.TEXT: TypeSupport.SUPPORTED,
        }

    def file_extension(self) -> str:
        return "sh"

    def initial_dependencies(self) -> list[str]:
        return []

    def needs_selector(self):
        return False

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.DEFAULT_PARAMETERS,
            Construct.GLOBAL_VARIABLES,
        }

    def is_source_file(self, file: Path) -> bool:
        return file.suffix == ""

    def submission_file(self) -> str:
        return submission_name(self)

    def compilation(self, files: list[str]) -> CallbackResult:
        submission = submission_file(self)
        main_file = list(filter(lambda x: x == submission, files))
        if main_file:
            return ["bash", "-n", main_file[0]], files
        else:
            return [], files

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        return ["bash", file, *arguments]

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        regex = re.compile(
            f"{EXECUTION_PREFIX}_[0-9]+_[0-9]+\\."
            f"{self.file_extension()}: [a-zA-Z_]+ [0-9]+:"
        )
        script = rf"{submission_file(self)}: (regel|rule) (\d+)"
        stacktrace = re.sub(script, r"<code>:\2", stacktrace)
        stacktrace = regex.sub("<testcode>:", stacktrace).replace(
            submission_file(self), "<code>"
        )
        regex = re.compile(
            f"{EXECUTION_PREFIX}_[0-9]+_[0-9]+\\." f"{self.file_extension()}"
        )
        return regex.sub("<testcode>", stacktrace)

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.bash import linter

        assert self.config
        return linter.run_shellcheck(self.config.dodona, remaining)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.bash import generators

        return generators.convert_statement(statement)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.bash import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_encoder(self, values: list[Value]) -> str:
        from tested.languages.bash import generators

        return generators.convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {"text": "str", "char": "str", "string": "str"},  # type: ignore
            "prompt": "$",
        }
