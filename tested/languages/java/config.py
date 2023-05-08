import logging
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Tuple

from tested.dodona import AnnotateCode, Message
from tested.languages.config import CallbackResult, Command, Language
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.languages.utils import (
    jvm_cleanup_stacktrace,
    jvm_memory_limit,
    jvm_stderr,
    limit_output,
)
from tested.serialisation import FunctionCall, Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


logger = logging.getLogger(__name__)


class Java(Language):
    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        return {
            "namespace": "pascal_case",
            "function": "camel_case",
            "identifier": "camel_case",
            "global_identifier": "macro_case",
            "property": "camel_case",
            "class": "pascal_case",
        }

    def compilation(self, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return ["javac", "-cp", ".", *others], file_filter

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        limit = jvm_memory_limit(self.config)
        return ["java", f"-Xmx{limit}", "-cp", ".", Path(file).stem, *arguments]

    def linter(self, remaining: float) -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.java import linter

        return linter.run_checkstyle(self.config.dodona, remaining)

    def cleanup_stacktrace(self, traceback: str) -> str:
        return jvm_cleanup_stacktrace(traceback, submission_file(self))

    def compiler_output(
        self, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        return (
            [],
            [],
            limit_output(stdout),
            jvm_cleanup_stacktrace(stderr, submission_file(self)),
        )

    def stderr(self, stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        return jvm_stderr(self, stderr)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.java import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.java import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: List[str]) -> str:
        from tested.languages.java import generators

        return generators.convert_selector(contexts)

    def generate_check_function(self, name: str, function: FunctionCall) -> str:
        from tested.languages.java import generators

        return generators.convert_check_function(function)

    def generate_encoder(self, values: List[Value]) -> str:
        from tested.languages.java import generators

        return generators.convert_encoder(values)
