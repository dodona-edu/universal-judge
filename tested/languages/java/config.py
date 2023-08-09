import logging
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Mapping, Optional, Set, Tuple

from tested.datatypes import AllTypes, ExpressionTypes
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.config import CallbackResult, Command, Language
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    submission_file,
)
from tested.languages.utils import jvm_cleanup_stacktrace, jvm_memory_limit
from tested.serialisation import FunctionCall, Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit


logger = logging.getLogger(__name__)


class Java(Language):
    def initial_dependencies(self) -> List[str]:
        return ["Values.java", "EvaluationResult.java"]

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "java"

    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        return {
            "namespace": "pascal_case",
            "function": "camel_case",
            "identifier": "camel_case",
            "global_identifier": "macro_case",
            "property": "camel_case",
            "class": "pascal_case",
        }

    def supported_constructs(self) -> Set[Construct]:
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

    def datatype_support(self) -> Mapping[AllTypes, TypeSupport]:
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

    def map_type_restrictions(self) -> Optional[Set[ExpressionTypes]]:
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

    def set_type_restrictions(self) -> Optional[Set[ExpressionTypes]]:
        return self.map_type_restrictions()

    def compilation(self, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return ["javac", "-cp", ".", *others], file_filter

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        assert self.config
        limit = jvm_memory_limit(self.config)
        return ["java", f"-Xmx{limit}", "-cp", ".", Path(file).stem, *arguments]

    def linter(self, remaining: float) -> Tuple[List[Message], List[AnnotateCode]]:
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

    def generate_selector(self, contexts: List[str]) -> str:
        from tested.languages.java import generators

        return generators.convert_selector(contexts)

    def generate_check_function(self, name: str, function: FunctionCall) -> str:
        from tested.languages.java import generators

        return generators.convert_check_function(function)

    def generate_encoder(self, values: List[Value]) -> str:
        from tested.languages.java import generators

        return generators.convert_encoder(values)
