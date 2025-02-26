import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import (
    AdvancedObjectTypes,
    AllTypes,
    BasicStringTypes,
    ExpressionTypes,
)
from tested.dodona import AnnotateCode, Message
from tested.features import Construct, TypeSupport
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    Conventionable,
    NamingConventions,
    submission_file,
    submission_name,
)
from tested.languages.language import (
    CallbackResult,
    Command,
    Language,
    TypeDeclarationMetadata,
)
from tested.languages.utils import cleanup_description
from tested.serialisation import Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit

logger = logging.getLogger(__name__)


class JavaScript(Language):
    def initial_dependencies(self) -> list[str]:
        return ["values.js"]

    def needs_selector(self):
        return False

    def file_extension(self) -> str:
        return "js"

    def comment(self, text: str) -> str:
        return f"// {text}"

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {
            "namespace": "camel_case",
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
            Construct.NAMED_ARGUMENTS,
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
            "dictionary": "supported",
            "object": "supported",
            "nothing": "supported",
            "undefined": "supported",
            "null": "supported",
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
            "array": "reduced",
            "list": "reduced",
            "tuple": "reduced",
        }

    def collection_restrictions(self) -> dict[AllTypes, set[ExpressionTypes]]:
        return {AdvancedObjectTypes.OBJECT: {BasicStringTypes.TEXT}}

    def compilation(self, files: list[str]) -> CallbackResult:
        submission = submission_file(self)
        main_file = list(filter(lambda x: x == submission, files))
        if main_file:
            return ["node", "--check", main_file[0]], files
        else:
            return [], files

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        return ["node", file, *arguments]

    def modify_solution(self, solution: Path):
        # import local to prevent errors
        from tested.judge.utils import run_command

        assert self.config

        parse_file = str(Path(__file__).parent / "parseAst.js")
        output = run_command(
            solution.parent,
            timeout=None,
            command=["node", parse_file, str(solution.absolute())],
            check=True,
        )
        assert output, "Missing output from JavaScript's modify_solution"
        namings = output.stdout.strip()
        with open(solution, "a") as file:
            print(f"\nmodule.exports = {{{namings}}};", file=file)

        # Add strict mode to the script.
        with open(solution, "r") as file:
            non_strict = file.read()
        with open(solution, "w") as file:
            file.write('"use strict";\n\n' + non_strict)
        self.config.dodona.source_offset -= 2

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.javascript import linter

        assert self.config
        return linter.run_eslint(self.config.dodona, remaining)

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        assert self.config
        # What this does:
        # 1a. While inside the submission code, replace all references to the location with <code>
        # 1b. Remove any "submission.SOMETHING" -> "SOMETHING"
        # 2. Once we encounter a line with the execution location, skip all lines.
        execution_submission_location_regex = f"{self.config.dodona.workdir}/{EXECUTION_PREFIX}[_0-9]+/{submission_file(self)}"
        submission_location = (
            self.config.dodona.workdir / "common" / submission_file(self)
        )
        compilation_submission_location = str(submission_location.resolve())
        execution_location_regex = f"{self.config.dodona.workdir}/{EXECUTION_PREFIX}[_0-9]+/{EXECUTION_PREFIX}[_0-9]+.js"
        submission_namespace = f"{submission_name(self)}."

        resulting_lines = ""
        for line in stacktrace.splitlines(keepends=True):
            # If we encounter an execution location, we are done.
            if re.search(execution_location_regex, line):
                break

            # Replace any reference to the submission.
            line = re.sub(execution_submission_location_regex, "<code>", line)
            line = line.replace(compilation_submission_location, "<code>")
            # Remove any references of the form "submission.SOMETHING"
            line = line.replace(submission_namespace, "")

            resulting_lines += line

        return resulting_lines

    def cleanup_description(self, statement: str) -> str:
        statement = cleanup_description(self, statement)
        await_regex = re.compile(r"await\s+")
        return await_regex.sub("", statement)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.javascript import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.javascript import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_encoder(self, values: list[Value]) -> str:
        from tested.languages.javascript import generators

        return generators.convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {  # type: ignore
                "integer": "number",
                "real": "number",
                "char": "string",
                "text": "string",
                "string": "string",
                "boolean": "boolean",
                "sequence": "array",
                "set": "set",
                "map": "object",
                "nothing": "null",
                "undefined": "undefined",
                "int8": "number",
                "uint8": "number",
                "int16": "number",
                "uint16": "number",
                "int32": "number",
                "uint32": "number",
                "int64": "number",
                "uint64": "number",
                "bigint": "number",
                "single_precision": "number",
                "double_precision": "number",
                "double_extended": "number",
                "fixed_precision": "number",
                "array": "array",
                "list": "array",
                "any": "object",
            },
            "nested": ("<", ">"),
            "exception": "Error",
        }
