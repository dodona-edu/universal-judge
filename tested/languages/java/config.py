import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import AllTypes, ExpressionTypes
from tested.dodona import AnnotateCode, Message
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
from tested.languages.utils import jvm_cleanup_stacktrace, jvm_memory_limit
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


logger = logging.getLogger(__name__)


class Java(Language):
    def initial_dependencies(self) -> list[str]:
        return ["Values.java", "EvaluationResult.java"]

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "java"

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
            "uint8": "reduced",
            "int16": "supported",
            "uint16": "reduced",
            "int32": "supported",
            "uint32": "reduced",
            "int64": "supported",
            "uint64": "reduced",
            "bigint": "supported",
            "single_precision": "supported",
            "double_precision": "supported",
            "double_extended": "reduced",
            "fixed_precision": "supported",
            "array": "supported",
            "list": "supported",
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
        return ["javac", "-cp", ".", *others], file_filter

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        assert self.config
        limit = jvm_memory_limit(self.config)
        return ["java", f"-Xmx{limit}", "-cp", ".", Path(file).stem, *arguments]

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.java import linter

        assert self.config
        return linter.run_checkstyle(self.config.dodona, remaining)

    def cleanup_stacktrace(self, traceback: str) -> str:
        return jvm_cleanup_stacktrace(traceback, submission_file(self))

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.java import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.java import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: list[str]) -> str:
        from tested.languages.java import generators

        return generators.convert_selector(contexts)

    def generate_encoder(self, values: list[Value]) -> str:
        from tested.languages.java import generators

        return generators.convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {
                "integer": "int",
                "real": "double",
                "char": "char",
                "text": "String",
                "boolean": "boolean",
                "sequence": "List",
                "set": "Set",
                "map": "Map",
                "nothing": "Void",
                "undefined": "Void",
                "int8": "byte",
                "uint8": "short",
                "int16": "short",
                "uint16": "int",
                "int32": "int",
                "uint32": "long",
                "int64": "long",
                "uint64": "BigInteger",
                "bigint": "BigInteger",
                "single_precision": "float",
                "double_precision": "double",
                "double_extended": "BigDecimal",
                "fixed_precision": "BigDecimal",
                "list": "List",
                "any": "Object",
                "array": False,
            },
            "inner_names": {
                "boolean": "Boolean",
                "char": "Character",
                "integer": "Integer",
                "real": "Double",
                "single_precision": "Float",
                "double_precision": "Double",
                "int8": "Byte",
                "uint8": "Short",
                "int16": "Short",
                "uint16": "Integer",
                "int32": "Integer",
                "uint32": "Long",
                "int64": "Long",
            },
            "nested": ("<", ">"),
            "nested_overrides": {"array": ("[", "]")},  # type: ignore
        }

    def is_void_method(self, name: str) -> bool:
        regex = rf"void\s+{name}"
        assert self.config
        the_source = self.config.dodona.source.read_text()
        return re.search(regex, the_source) is not None
