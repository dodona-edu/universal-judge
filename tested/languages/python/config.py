import logging
import os
import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import AllTypes, ExpressionTypes
from tested.dodona import AnnotateCode, Message, Severity
from tested.features import Construct, TypeSupport
from tested.languages.config import (
    CallbackResult,
    Command,
    Language,
    TypeDeclarationMetadata,
)
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit

logger = logging.getLogger(__name__)


def _executable():
    if os.name == "nt":
        return "python"
    else:
        return "python3"


class Python(Language):
    def initial_dependencies(self) -> list[str]:
        return ["values.py", "evaluation_utils.py"]

    def needs_selector(self):
        return False

    def supports_debug_information(self) -> bool:
        return True

    def file_extension(self) -> str:
        return "py"

    def get_string_quote(self):
        return "'"

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {"class": "pascal_case", "global_identifier": "macro_case"}

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.OBJECTS,
            Construct.EXCEPTIONS,
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.HETEROGENEOUS_COLLECTIONS,
            Construct.HETEROGENEOUS_ARGUMENTS,
            Construct.NAMED_ARGUMENTS,
            Construct.EVALUATION,
            Construct.DEFAULT_PARAMETERS,
            Construct.GLOBAL_VARIABLES,
        }

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return {  # type: ignore
            "integer": "supported",
            "real": "supported",
            "char": "reduced",
            "text": "supported",
            "string": "supported",
            "boolean": "supported",
            "sequence": "supported",
            "set": "supported",
            "map": "supported",
            "nothing": "supported",
            "undefined": "reduced",
            "null": "reduced",
            "int8": "reduced",
            "uint8": "reduced",
            "int16": "reduced",
            "uint16": "reduced",
            "int32": "reduced",
            "uint32": "reduced",
            "int64": "reduced",
            "uint64": "reduced",
            "bigint": "supported",
            "single_precision": "reduced",
            "double_precision": "reduced",
            "double_extended": "reduced",
            "fixed_precision": "supported",
            "array": "reduced",
            "list": "supported",
            "tuple": "supported",
        }

    def map_type_restrictions(self) -> set[ExpressionTypes] | None:
        return {  # type: ignore
            "integer",
            "real",
            "text",
            "boolean",
            "tuple",
            "bigint",
            "fixed_precision",
            "function_calls",
            "identifiers",
        }

    def set_type_restrictions(self) -> set[ExpressionTypes] | None:
        return self.map_type_restrictions()

    def compilation(self, files: list[str]) -> CallbackResult:
        result = [x.replace(".py", ".pyc") for x in files]
        return [
            _executable(),
            "-W",
            "ignore",
            "-m",
            "compileall",
            "-q",
            "-b",
            ".",
        ], result

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        return [_executable(), "-u", file, *arguments]

    def compiler_output(
        self, stdout: str, stderr: str
    ) -> tuple[list[Message], list[AnnotateCode], str, str]:
        if match := re.search(r".*: (.+Error): (.+) \(<code>, line (\d+)\)", stdout):
            error = match.group(1)
            message = match.group(2)
            line = match.group(3)
            return (
                [],
                [
                    AnnotateCode(
                        row=int(line), text=f"{error}: {message}", type=Severity.ERROR
                    )
                ],
                stdout,
                stderr,
            )
        else:
            return [], [], stdout, stderr

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.python import linter

        assert self.config
        return linter.run_pylint(self.config.dodona, remaining)

    # Idea and original code: dodona/judge-pythia
    def cleanup_stacktrace(self, stacktrace_str: str) -> str:
        context_file_regex = re.compile(r"context_[0-9]+_[0-9]+\.py")
        file_line_regex = re.compile(rf"\({submission_file(self)}, line (\d+)\)")
        file_full_regex = re.compile(rf'File "./{submission_file(self)}", line (\d+)')
        stacktrace = stacktrace_str.splitlines(True)

        skip_line, lines = False, []
        for line in stacktrace:
            line = line.strip("\n")
            logger.debug(line)

            if not line:
                continue

            if line.startswith(
                "During handling of the above exception, another exception occurred:"
            ):
                lines = []
                continue

            # skip line if not a new File line is started
            if context_file_regex.search(line):
                skip_line = True
                continue
            elif skip_line and (not line.startswith(" ") or "File" in line):
                skip_line = False
            elif skip_line:
                continue

            # replace references to local names
            if file_full_regex.search(line):
                line = file_full_regex.sub(r'File "<code>:\1"', line)
            elif f'File "./{submission_file(self)}"' in line:
                line = line.replace(
                    f'File "./{submission_file(self)}"', 'File "<code>"'
                )
            elif line.startswith("*** Error compiling"):
                line = line.replace(f"./{submission_file(self)}", "<code>")
            elif file_line_regex.search(line):
                line = file_line_regex.sub(r"(<code>:\1)", line)
            elif 'File "<string>"' in line:
                line = line.replace('File "<string>"', 'File "<code>"')
            elif 'File "<doctest>"' in line:
                continue
            elif 'File "' in line:
                skip_line = True
                continue
            skip_line = False

            # replace references to modules
            if ", in <module>" in line:
                line = line.replace(", in <module>", "")

            lines.append(line + "\n")
        return "".join(lines)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.python import generators

        return generators.convert_statement(statement)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.python import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_encoder(self, values: list[Value]) -> str:
        from tested.languages.python import generators

        return generators.convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {  # type: ignore
                "integer": "int",
                "real": "float",
                "char": "str",
                "text": "str",
                "string": "str",
                "boolean": "bool",
                "sequence": "list",
                "set": "set",
                "map": "dict",
                "nothing": "None",
                "undefined": "None",
                "int8": "int",
                "uint8": "int",
                "int16": "int",
                "uint16": "int",
                "int32": "int",
                "uint32": "int",
                "int64": "int",
                "uint64": "int",
                "bigint": "int",
                "single_precision": "float",
                "double_precision": "float",
                "double_extended": "Decimal",
                "fixed_precision": "Decimal",
                "array": "list",
                "list": "list",
                "tuple": "tuple",
                "any": "Any",
            },
            "prompt": ">>>",
        }
