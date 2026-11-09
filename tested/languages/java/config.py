import logging
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
            Construct.DEFAULT_PARAMETERS,
            Construct.GLOBAL_VARIABLES,
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
            AdvancedSequenceTypes.TUPLE: TypeSupport.REDUCED,
            BasicSequenceTypes.SET: TypeSupport.SUPPORTED,
            BasicObjectTypes.MAP: TypeSupport.SUPPORTED,
            AdvancedObjectTypes.DICTIONARY: TypeSupport.SUPPORTED,
            AdvancedObjectTypes.OBJECT: TypeSupport.REDUCED,
            BasicNothingTypes.NOTHING: TypeSupport.SUPPORTED,
            AdvancedNothingTypes.UNDEFINED: TypeSupport.REDUCED,
            AdvancedNothingTypes.NULL: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_8: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_8: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_16: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_16: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_32: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_32: TypeSupport.REDUCED,
            AdvancedNumericTypes.INT_64: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.U_INT_64: TypeSupport.REDUCED,
            AdvancedNumericTypes.BIG_INT: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.SINGLE_PRECISION: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.DOUBLE_PRECISION: TypeSupport.SUPPORTED,
            AdvancedNumericTypes.DOUBLE_EXTENDED: TypeSupport.REDUCED,
            AdvancedNumericTypes.FIXED_PRECISION: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.ARRAY: TypeSupport.SUPPORTED,
            AdvancedSequenceTypes.LIST: TypeSupport.SUPPORTED,
        }

    def collection_restrictions(self) -> dict[AllTypes, set[ExpressionTypes]]:
        restrictions = {
            BasicNumericTypes.INTEGER,
            BasicNumericTypes.REAL,
            AdvancedStringTypes.CHAR,
            BasicStringTypes.TEXT,
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
        # javac runs once in a fresh JVM, so the time C2 spends optimising the
        # compiler never pays off. Stopping at C1 saves CPU.
        return [
            "javac",
            "-J-XX:TieredStopAtLevel=1",
            "-cp",
            ".",
            *others,
        ], file_filter

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        assert self.config
        limit = jvm_memory_limit(self.config)
        return ["java", f"-Xmx{limit}", "-cp", ".", Path(file).stem, *arguments]

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.java import linter

        assert self.config
        return linter.run_checkstyle(self.config.dodona, remaining)

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        return jvm_cleanup_stacktrace(stacktrace, submission_file(self))

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.java import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: PreparedExecutionUnit) -> str:
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
                BasicNumericTypes.INTEGER: "int",
                BasicNumericTypes.REAL: "double",
                AdvancedStringTypes.CHAR: "char",
                BasicStringTypes.TEXT: "String",
                AdvancedStringTypes.STRING: "String",
                BasicBooleanTypes.BOOLEAN: "boolean",
                BasicSequenceTypes.SEQUENCE: "List",
                BasicSequenceTypes.SET: "Set",
                BasicObjectTypes.MAP: "Map",
                AdvancedObjectTypes.DICTIONARY: "Map",
                AdvancedObjectTypes.OBJECT: "Map",
                BasicNothingTypes.NOTHING: "Void",
                AdvancedNothingTypes.UNDEFINED: "Void",
                AdvancedNumericTypes.INT_8: "byte",
                AdvancedNumericTypes.U_INT_8: "short",
                AdvancedNumericTypes.INT_16: "short",
                AdvancedNumericTypes.U_INT_16: "int",
                AdvancedNumericTypes.INT_32: "int",
                AdvancedNumericTypes.U_INT_32: "long",
                AdvancedNumericTypes.INT_64: "long",
                AdvancedNumericTypes.U_INT_64: "BigInteger",
                AdvancedNumericTypes.BIG_INT: "BigInteger",
                AdvancedNumericTypes.SINGLE_PRECISION: "float",
                AdvancedNumericTypes.DOUBLE_PRECISION: "double",
                AdvancedNumericTypes.DOUBLE_EXTENDED: "BigDecimal",
                AdvancedNumericTypes.FIXED_PRECISION: "BigDecimal",
                AdvancedSequenceTypes.LIST: "List",
                BasicStringTypes.ANY: "Object",
                AdvancedSequenceTypes.ARRAY: (False, "array"),
            },
            "inner_names": {
                BasicBooleanTypes.BOOLEAN: "Boolean",
                AdvancedStringTypes.CHAR: "Character",
                BasicNumericTypes.INTEGER: "Integer",
                BasicNumericTypes.REAL: "Double",
                AdvancedNumericTypes.SINGLE_PRECISION: "Float",
                AdvancedNumericTypes.DOUBLE_PRECISION: "Double",
                AdvancedNumericTypes.INT_8: "Byte",
                AdvancedNumericTypes.U_INT_8: "Short",
                AdvancedNumericTypes.INT_16: "Short",
                AdvancedNumericTypes.U_INT_16: "Integer",
                AdvancedNumericTypes.INT_32: "Integer",
                AdvancedNumericTypes.U_INT_32: "Long",
                AdvancedNumericTypes.INT_64: "Long",
            },
            "nested": ("<", ">"),
            "nested_overrides": {AdvancedSequenceTypes.ARRAY: ("[", "]")},
            "exception": "Exception",
        }

    def is_void_method(self, name: str) -> bool:
        regex = rf"void\s+{re.escape(name)}(?!\w)"
        assert self.config
        the_source = self.config.dodona.source.read_text()
        return re.search(regex, the_source) is not None
