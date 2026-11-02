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
from tested.dodona import AnnotateCode, Message, Status
from tested.features import Construct, TypeSupport
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    Conventionable,
    NamingConventions,
    conventionalize_namespace,
    submission_file,
)
from tested.languages.language import (
    CallbackResult,
    Command,
    Language,
    TypeDeclarationMetadata,
)
from tested.languages.utils import jvm_cleanup_stacktrace, jvm_memory_limit
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit

logger = logging.getLogger(__name__)


def get_executable(name):
    if os.name == "nt":
        return f"{name}.bat"
    return name


# Both the compiler and the submission run once in a fresh JVM, so the time C2
# spends optimising hot code never pays off. Stopping at C1 saves CPU.
JIT_FLAGS = ["-XX:TieredStopAtLevel=1"]


class Kotlin(Language):
    def initial_dependencies(self) -> list[str]:
        return ["Values.kt", "EvaluationResult.kt"]

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "kt"

    def comment(self, text: str) -> str:
        return f"// {text}"

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {
            "namespace": "pascal_case",
            "function": "camel_case",
            "identifier": "camel_case",
            "global_identifier": "macro_case",
            "property": "camel_case",
            "class": "pascal_case",
        }

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.OBJECTS,
            Construct.EXCEPTIONS,
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.HETEROGENEOUS_COLLECTIONS,
            Construct.HETEROGENEOUS_ARGUMENTS,
            Construct.EVALUATION,
            Construct.NAMED_ARGUMENTS,
            Construct.DEFAULT_PARAMETERS,
            Construct.GLOBAL_VARIABLES,
        }

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return {
            BasicNumericTypes.INTEGER: TypeSupport.SUPPORTED,
            BasicNumericTypes.REAL: TypeSupport.SUPPORTED,
            AdvancedStringTypes.CHAR: TypeSupport.SUPPORTED,
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
            AdvancedNumericTypes.DOUBLE_EXTENDED: TypeSupport.REDUCED,
            AdvancedNumericTypes.FIXED_PRECISION: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.ARRAY: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.LIST: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.TUPLE: TypeSupport.REDUCED,
        }

    def collection_restrictions(self) -> dict[AllTypes, set[ExpressionTypes]]:
        restrictions = {
            BasicNumericTypes.INTEGER,
            BasicNumericTypes.REAL,
            AdvancedStringTypes.CHAR,
            BasicStringTypes.TEXT,
            AdvancedStringTypes.STRING,
            BasicBooleanTypes.BOOLEAN,
            BasicSequenceTypes.SEQUENCE,
            BasicSequenceTypes.SET,
            BasicObjectTypes.MAP,
            AdvancedNumericTypes.INT_8,
            AdvancedNumericTypes.INT_16,
            AdvancedNumericTypes.INT_32,
            AdvancedNumericTypes.INT_64,
            AdvancedNumericTypes.SINGLE_PRECISION,
            AdvancedNumericTypes.DOUBLE_PRECISION,
            AdvancedNumericTypes.FIXED_PRECISION,
            AdvancedSequenceTypes.ARRAY,
            AdvancedSequenceTypes.LIST,
            ComplexExpressionTypes.FUNCTION_CALLS,
            ComplexExpressionTypes.IDENTIFIERS,
        }
        return {
            BasicObjectTypes.MAP: restrictions,
            BasicSequenceTypes.SET: restrictions,
        }

    def compilation(self, files: list[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return [
            get_executable("kotlinc"),
            "-J-Xmx192M",
            *(f"-J{flag}" for flag in JIT_FLAGS),
            "-nowarn",
            "-jvm-target",
            "11",
            "-cp",
            ".",
            *others,
        ], file_filter

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        assert self.config
        limit = jvm_memory_limit(self.config)
        return [
            get_executable("kotlin"),
            f"-J-Xmx{limit}",
            *(f"-J{flag}" for flag in JIT_FLAGS),
            "-cp",
            ".",
            Path(file).stem,
            *arguments,
        ]

    def modify_solution(self, solution: Path):
        with open(solution, "r") as file:
            contents = file.read()
        # We use regex to find the main function.
        # First, check if we have a no-arg main function.
        # If so, replace it with a renamed main function that does have args.
        # Needed for main outside class
        no_args = re.compile(r"fun(\s+)main(\s*)\((\s*)\)")
        replacement = r"fun\1solutionMain\2(args: Array<String> = emptyArray()\3)"
        contents, nr = re.subn(no_args, replacement, contents, count=1)
        if nr == 0:
            # There was no main function without arguments. Now we try a main
            # function with arguments.
            with_args = re.compile(
                r"fun(\s+)main(\s*)\((\s*[^\s]*\s*:\s*" r"Array\s*<\s*String\s*>)"
            )
            replacement = r"fun\1solutionMain\2(\3"
            contents = re.sub(with_args, replacement, contents, count=1)
        with open(solution, "w") as file:
            file.write(contents)

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.kotlin import linter

        assert self.config
        return linter.run_ktlint(self.config.dodona, remaining)

    def find_main_file(self, files: list[Path], name: str) -> Path | Status:
        logger.debug("Finding %s in %s", name, files)
        main_or_status = Language.find_main_file(self, files, name + "Kt")
        if isinstance(main_or_status, Path):
            return main_or_status
        else:
            return Language.find_main_file(self, files, name)

    def filter_dependencies(self, files: list[Path], context_name: str) -> list[Path]:
        def filter_function(file_path: Path) -> bool:
            # We don't want files for contexts that are not the one we use.
            prefix = conventionalize_namespace(self, EXECUTION_PREFIX)
            file = str(file_path)
            is_context = file.startswith(prefix)
            is_our_context = file.startswith((context_name + ".", context_name + "$"))
            return not is_context or is_our_context

        return [x for x in files if filter_function(x)]

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        return jvm_cleanup_stacktrace(stacktrace, submission_file(self))

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.kotlin import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.kotlin import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: list[str]) -> str:
        from tested.languages.kotlin import generators

        return generators.convert_selector(contexts)

    def generate_encoder(self, values: list[Value]) -> str:
        from tested.languages.kotlin import generators

        return generators.convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {
                BasicNumericTypes.INTEGER: "Int",
                BasicNumericTypes.REAL: "Double",
                AdvancedStringTypes.CHAR: "Char",
                BasicStringTypes.TEXT: "String",
                AdvancedStringTypes.STRING: "String",
                BasicBooleanTypes.BOOLEAN: "Boolean",
                BasicSequenceTypes.SEQUENCE: "List",
                BasicSequenceTypes.SET: "Set",
                BasicObjectTypes.MAP: "Map",
                BasicNothingTypes.NOTHING: "Void",
                AdvancedNothingTypes.UNDEFINED: "Void",
                AdvancedNumericTypes.INT_8: "Byte",
                AdvancedNumericTypes.U_INT_8: "UByte",
                AdvancedNumericTypes.INT_16: "Short",
                AdvancedNumericTypes.U_INT_16: "UShort",
                AdvancedNumericTypes.INT_32: "Int",
                AdvancedNumericTypes.U_INT_32: "UInt",
                AdvancedNumericTypes.INT_64: "Long",
                AdvancedNumericTypes.U_INT_64: "ULong",
                AdvancedNumericTypes.BIG_INT: "BigInteger",
                AdvancedNumericTypes.SINGLE_PRECISION: "Float",
                AdvancedNumericTypes.DOUBLE_PRECISION: "Double",
                AdvancedNumericTypes.DOUBLE_EXTENDED: "BigDecimal",
                AdvancedNumericTypes.FIXED_PRECISION: "BigDecimal",
                AdvancedSequenceTypes.ARRAY: "Array",
                AdvancedSequenceTypes.LIST: "List",
                BasicStringTypes.ANY: "Any",
            },
            "nested": ("<", ">"),
            "exception": "Exception",
        }
