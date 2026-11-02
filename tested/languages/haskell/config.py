import functools
import re
import shutil
import sys
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicSequenceTypes,
    BasicStringTypes,
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
from tested.languages.utils import cleanup_description as strip_submission_prefix
from tested.languages.utils import executable_name, haskell_solution
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


HELPER_MODULES = ["Values", "EvaluationUtils"]
TEMPLATES_DIRECTORY = Path(__file__).parent / "templates"

# The dodona-tested image compiles the helper modules ahead of time.
PRECOMPILED_DIRECTORY = Path("/usr/local/share/tested/haskell/static")


@functools.cache
def precompiled_helpers() -> Path | None:
    """
    Find the precompiled helper modules of the image, if it has them.

    They are only used if they were built from the same sources as the templates of
    this judge: otherwise GHC would recompile them, and units that compile in
    parallel share the hard-linked files.
    """
    directory = PRECOMPILED_DIRECTORY
    for module in HELPER_MODULES:
        if not all((directory / f"{module}.{ext}").is_file() for ext in ("hi", "o")):
            return None
        source = directory / f"{module}.hs"
        template = TEMPLATES_DIRECTORY / f"{module}.hs"
        if not source.is_file() or source.read_bytes() != template.read_bytes():
            return None
    return directory


@functools.cache
def linker_flags() -> list[str]:
    # lld links a lot faster than GNU ld, the default of GHC.
    if sys.platform.startswith("linux") and shutil.which("ld.lld"):
        return ["-optl-fuse-ld=lld"]
    return []


class Haskell(Language):
    def _precompiled_helpers(self) -> Path | None:
        assert self.config
        # The image compiles them with -O0.
        if self.config.options.compiler_optimizations:
            return None
        return precompiled_helpers()

    def initial_dependencies(self) -> list[str]:
        dependencies = [f"{module}.hs" for module in HELPER_MODULES]
        if self._precompiled_helpers():
            dependencies += [
                f"{module}.{ext}" for module in HELPER_MODULES for ext in ("hi", "o")
            ]
        return dependencies

    def path_to_dependencies(self) -> list[Path]:
        assert self.config
        paths = [
            self.config.dodona.judge / "tested" / "languages" / "haskell" / "templates"
        ]
        if directory := self._precompiled_helpers():
            paths.insert(0, directory)
        return paths

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "hs"

    def comment(self, text: str) -> str:
        return f"-- {text}"

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {
            "namespace": "pascal_case",
            "identifier": "camel_case",
            "global_identifier": "camel_case",
            "function": "camel_case",
        }

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return {
            BasicNumericTypes.INTEGER: TypeSupport.SUPPORTED,
            BasicNumericTypes.REAL: TypeSupport.SUPPORTED,
            AdvancedStringTypes.CHAR: TypeSupport.SUPPORTED,
            AdvancedStringTypes.STRING: TypeSupport.SUPPORTED,
            BasicStringTypes.TEXT: TypeSupport.SUPPORTED,
            BasicBooleanTypes.BOOLEAN: TypeSupport.SUPPORTED,
            BasicSequenceTypes.SEQUENCE: TypeSupport.SUPPORTED,
            BasicNothingTypes.NOTHING: TypeSupport.SUPPORTED,
            AdvancedNothingTypes.UNDEFINED: TypeSupport.REDUCED,
            AdvancedNothingTypes.NULL: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_8: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_8: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.INT_16: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_16: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.INT_32: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_32: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.INT_64: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_64: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.BIG_INT: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.SINGLE_PRECISION: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.DOUBLE_PRECISION: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.LIST: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.TUPLE: TypeSupport.SUPPORTED,
        }

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.EXCEPTIONS,
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.EVALUATION,
            Construct.GLOBAL_VARIABLES,
            Construct.HETEROGENEOUS_ARGUMENTS,
        }

    def compilation(self, files: list[str]) -> CallbackResult:
        main_ = files[-1]
        exec_ = main_.rstrip(".hs")
        assert self.config
        return [
            "ghc",
            "-v0",
            "-fno-cse",
            "-fno-full-laziness",
            "-O3" if self.config.options.compiler_optimizations else "-O0",
            *linker_flags(),
            main_,
            "-main-is",
            exec_,
            # A larger allocation area makes ghc collect garbage less often.
            "+RTS",
            "-A32m",
            "-RTS",
        ], [executable_name(exec_)]

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        local_file = cwd / file
        return [str(local_file.absolute()), *arguments]

    def modify_solution(self, solution: Path):
        haskell_solution(self, solution)

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.haskell import linter

        assert self.config
        return linter.run_hlint(self.config.dodona, remaining)

    def cleanup_description(self, statement: str) -> str:
        return strip_submission_prefix(self, statement)

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        assert self.config
        offset = self.config.dodona.source_offset
        filename = submission_file(self)
        diagnostic_regex = re.compile(r"^<code>:\d+:\d+: (error|warning)")
        context_file_regex = re.compile(r"(Context[0-9]+|Selector)")
        compile_line_regex = re.compile(r"^([0-9]+)(\s*\|.*)$")
        hint_line_regex = re.compile(r"\(line (\d+)\)")
        type_conflict_regex = re.compile(
            r"Couldn't match expected type (.*) with actual type (.*)"
        )
        parse_module = r"error: parse error on input ‘module’"
        replace_module = r"error: unexpected ‘module’"
        traceback = stacktrace.splitlines()
        skip_line, in_diagnostic, lines = False, False, []
        for line in traceback:
            if not line or line == "undefined":
                continue

            # skip line if not a new File line is started
            if context_file_regex.search(line):
                skip_line = True
                continue
            elif skip_line and (line.startswith(" ") or line[0].isdigit()):
                match = type_conflict_regex.search(line)
                if match:
                    lines.append(
                        "Argument type conflict: Couldn't match expected type "
                        f"{match.group(2)} with actual type "
                        f"{match.group(1)}\n"
                    )
                continue

            # replace references to local names
            if filename in line:
                line = line.replace(f"./{filename}", "<code>").replace(
                    filename, "<code>"
                )
            elif "at " in line:
                skip_line = True
                continue
            skip_line = False

            if diagnostic_regex.match(line):
                in_diagnostic = True
            elif not (in_diagnostic and (line[0].isspace() or line[0].isdigit())):
                in_diagnostic = False

            if parse_module in line:
                line = line.replace(parse_module, replace_module)
            elif in_diagnostic:
                match = compile_line_regex.match(line)
                if match:
                    line = f"{int(match.group(1)) + offset}{match.group(2)}"
                else:
                    line = hint_line_regex.sub(
                        lambda hint: f"(line {int(hint.group(1)) + offset})", line
                    )

            lines.append(line + "\n")
        return "".join(lines)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.haskell import generators

        return generators.convert_statement(statement, display=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.haskell import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: list[str]) -> str:
        from tested.languages.haskell import generators

        return generators.convert_selector(contexts)

    def generate_encoder(self, values: list[Value]) -> str:
        from tested.languages.haskell import generators

        return generators.convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {
                BasicNumericTypes.INTEGER: "Int",
                BasicNumericTypes.REAL: "Double",
                AdvancedStringTypes.CHAR: "Char",
                AdvancedStringTypes.STRING: "String",
                BasicStringTypes.TEXT: "String",
                BasicBooleanTypes.BOOLEAN: "Bool",
                BasicNothingTypes.NOTHING: "Nothing",
                AdvancedNothingTypes.UNDEFINED: "Nothing",
                AdvancedNumericTypes.INT_8: "Data.Int.Int8",
                AdvancedNumericTypes.U_INT_8: "Data.Word.Word8",
                AdvancedNumericTypes.INT_16: "Data.Int.Int16",
                AdvancedNumericTypes.U_INT_16: "Data.Word.Word16",
                AdvancedNumericTypes.INT_32: "Data.Int.Int32",
                AdvancedNumericTypes.U_INT_32: "Data.Word.Word32",
                AdvancedNumericTypes.INT_64: "Data.Int.Int64",
                AdvancedNumericTypes.U_INT_64: "Data.Word.Word64",
                AdvancedNumericTypes.BIG_INT: "Integer",
                AdvancedNumericTypes.SINGLE_PRECISION: "Float",
                AdvancedNumericTypes.DOUBLE_PRECISION: "Double",
                BasicStringTypes.ANY: "Object",
                AdvancedSequenceTypes.LIST: (True, "Data.List"),
                AdvancedSequenceTypes.TUPLE: (True, "Data.Tuple"),
                BasicSequenceTypes.SEQUENCE: (True, "Data.List"),
            },
            "nested_overrides": {AdvancedSequenceTypes.TUPLE: ("(", ")")},
            "exception": "Exception",
        }
