import logging
import os
import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
    AdvancedObjectTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    ComplexExpressionTypes,
    ExpressionTypes,
)
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.conventionalize import (
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

    def comment(self, text: str) -> str:
        return f"# {text}"

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
        return {
            BasicNumericTypes.INTEGER: TypeSupport.SUPPORTED,
            BasicNumericTypes.REAL: TypeSupport.SUPPORTED,
            AdvancedStringTypes.CHAR: TypeSupport.REDUCED,
            BasicStringTypes.TEXT: TypeSupport.SUPPORTED,
            AdvancedStringTypes.STRING: TypeSupport.SUPPORTED,
            BasicBooleanTypes.BOOLEAN: TypeSupport.SUPPORTED,
            BasicSequenceTypes.SEQUENCE: TypeSupport.SUPPORTED,
            BasicSequenceTypes.SET: TypeSupport.SUPPORTED,
            BasicObjectTypes.MAP: TypeSupport.SUPPORTED,
            AdvancedObjectTypes.DICTIONARY: TypeSupport.SUPPORTED,
            AdvancedObjectTypes.OBJECT: TypeSupport.REDUCED,
            BasicNothingTypes.NOTHING: TypeSupport.SUPPORTED,
            AdvancedNothingTypes.UNDEFINED: TypeSupport.REDUCED,
            AdvancedNothingTypes.NULL: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_8: TypeSupport.REDUCED,
            AdvancedNumericTypes.U_INT_8: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_16: TypeSupport.REDUCED,
            AdvancedNumericTypes.U_INT_16: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_32: TypeSupport.REDUCED,
            AdvancedNumericTypes.U_INT_32: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_64: TypeSupport.REDUCED,
            AdvancedNumericTypes.U_INT_64: TypeSupport.REDUCED,
            AdvancedNumericTypes.BIG_INT: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.SINGLE_PRECISION: TypeSupport.REDUCED,
            AdvancedNumericTypes.DOUBLE_PRECISION: TypeSupport.REDUCED,
            AdvancedNumericTypes.DOUBLE_EXTENDED: TypeSupport.REDUCED,
            AdvancedNumericTypes.FIXED_PRECISION: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.ARRAY: TypeSupport.REDUCED,
            AdvancedSequenceTypes.LIST: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.TUPLE: TypeSupport.SUPPORTED,
        }

    def collection_restrictions(self) -> dict[AllTypes, set[ExpressionTypes]]:
        restrictions = {
            BasicNumericTypes.INTEGER,
            BasicNumericTypes.REAL,
            BasicStringTypes.TEXT,
            BasicBooleanTypes.BOOLEAN,
            AdvancedSequenceTypes.TUPLE,
            AdvancedNumericTypes.BIG_INT,
            AdvancedNumericTypes.FIXED_PRECISION,
            ComplexExpressionTypes.FUNCTION_CALLS,
            ComplexExpressionTypes.IDENTIFIERS,
        }
        return {
            BasicObjectTypes.MAP: restrictions,
            BasicSequenceTypes.SET: restrictions,
        }

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

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.python import linter

        assert self.config
        return linter.run_ruff(self.config.dodona, remaining)

    # Idea and original code: dodona/judge-pythia
    def cleanup_stacktrace(self, stacktrace: str) -> str:
        context_file_regex = re.compile(r"context_[0-9]+_[0-9]+\.py")
        file_line_regex = re.compile(rf"\({submission_file(self)}, line (\d+)\)")
        file_full_regex = re.compile(rf'File "./{submission_file(self)}", line (\d+)')
        stacktrace_split = stacktrace.splitlines(True)

        skip_line, lines = False, []
        for line in stacktrace_split:
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
            "names": {
                BasicNumericTypes.INTEGER: "int",
                BasicNumericTypes.REAL: "float",
                AdvancedStringTypes.CHAR: "str",
                BasicStringTypes.TEXT: "str",
                AdvancedStringTypes.STRING: "str",
                BasicBooleanTypes.BOOLEAN: "bool",
                BasicSequenceTypes.SEQUENCE: "list",
                BasicSequenceTypes.SET: "set",
                BasicObjectTypes.MAP: "dict",
                BasicNothingTypes.NOTHING: "None",
                AdvancedNothingTypes.UNDEFINED: "None",
                AdvancedNumericTypes.INT_8: "int",
                AdvancedNumericTypes.U_INT_8: "int",
                AdvancedNumericTypes.INT_16: "int",
                AdvancedNumericTypes.U_INT_16: "int",
                AdvancedNumericTypes.INT_32: "int",
                AdvancedNumericTypes.U_INT_32: "int",
                AdvancedNumericTypes.INT_64: "int",
                AdvancedNumericTypes.U_INT_64: "int",
                AdvancedNumericTypes.BIG_INT: "int",
                AdvancedNumericTypes.SINGLE_PRECISION: "float",
                AdvancedNumericTypes.DOUBLE_PRECISION: "float",
                AdvancedNumericTypes.DOUBLE_EXTENDED: "Decimal",
                AdvancedNumericTypes.FIXED_PRECISION: "Decimal",
                AdvancedSequenceTypes.ARRAY: "list",
                AdvancedSequenceTypes.LIST: "list",
                AdvancedSequenceTypes.TUPLE: "tuple",
                BasicStringTypes.ANY: "Any",
            },
            "prompt": ">>>",
            "exception": "Exception",
        }
