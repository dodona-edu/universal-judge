import logging
import os
import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import AllTypes, ExpressionTypes
from tested.dodona import AnnotateCode, Message, Status
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
    conventionalize_namespace,
    submission_file,
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


class Kotlin(Language):
    def initial_dependencies(self) -> list[str]:
        return ["Values.kt", "EvaluationResult.kt"]

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "kt"

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
        return {  # type: ignore
            "integer": "supported",
            "real": "supported",
            "char": "supported",
            "text": "supported",
            "boolean": "supported",
            "sequence": "supported",
            "set": "supported",
            "map": "supported",
            "nothing": "supported",
            "undefined": "reduced",
            "null": "reduced",
            "int8": "supported",
            "uint8": "supported",
            "int16": "supported",
            "uint16": "supported",
            "int32": "supported",
            "uint32": "supported",
            "int64": "supported",
            "uint64": "supported",
            "bigint": "supported",
            "single_precision": "supported",
            "double_precision": "supported",
            "double_extended": "reduced",
            "fixed_precision": "supported",
            "array": "supported",
            "list": "supported",
            "tuple": "reduced",
        }

    def map_type_restrictions(self) -> set[ExpressionTypes] | None:
        return {  # type: ignore
            "integer",
            "real",
            "char",
            "text",
            "boolean",
            "sequence",
            "set",
            "map",
            "int8",
            "int16",
            "int32",
            "int64",
            "single_precision",
            "double_precision",
            "fixed_precision",
            "array",
            "list",
            "function_calls",
            "identifiers",
        }

    def set_type_restrictions(self) -> set[ExpressionTypes] | None:
        return self.map_type_restrictions()

    def compilation(self, files: list[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return [
            get_executable("kotlinc"),
            f"-J-Xmx192M",
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

    def find_main_file(
        self, files: list[Path], name: str
    ) -> tuple[Path | None, Status]:
        logger.debug("Finding %s in %s", name, files)
        main, status = Language.find_main_file(self, files, name + "Kt")
        if status == Status.CORRECT:
            return main, status
        else:
            return Language.find_main_file(self, files, name)

    def filter_dependencies(self, files: list[Path], context_name: str) -> list[Path]:
        def filter_function(file_path: Path) -> bool:
            # We don't want files for contexts that are not the one we use.
            prefix = conventionalize_namespace(self, EXECUTION_PREFIX)
            file = str(file_path)
            is_context = file.startswith(prefix)
            is_our_context = file.startswith(context_name + ".") or file.startswith(
                context_name + "$"
            )
            return not is_context or is_our_context

        return list(x for x in files if filter_function(x))

    def cleanup_stacktrace(self, traceback: str) -> str:
        return jvm_cleanup_stacktrace(traceback, submission_file(self))

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
            "names": {  # type: ignore
                "integer": "Int",
                "real": "Double",
                "char": "Char",
                "text": "String",
                "boolean": "Boolean",
                "sequence": "List",
                "set": "Set",
                "map": "Map",
                "nothing": "Void",
                "undefined": "Void",
                "int8": "Byte",
                "uint8": "UByte",
                "int16": "Short",
                "uint16": "UShort",
                "int32": "Int",
                "uint32": "UInt",
                "int64": "Long",
                "uint64": "ULong",
                "bigint": "BigInteger",
                "single_precision": "Float",
                "double_precision": "Double",
                "double_extended": "BigDecimal",
                "fixed_precision": "BigDecimal",
                "array": "Array",
                "list": "List",
                "any": "Any",
            },
            "nested": ("<", ">"),
        }
