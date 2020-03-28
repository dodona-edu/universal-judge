import os
import re
from pathlib import Path
from typing import List, Tuple, Mapping, Optional

from humps import decamelize, depascalize

from . import python_linter
from ..config import Language, CallbackResult, TypeSupport
from ...configs import Bundle
from ...datatypes import (AdvancedNumericTypes as ant, AllTypes,
                          AdvancedSequenceTypes as ast)
from ...dodona import AnnotateCode, Severity, Message
from ...testplan import Plan
from ...utils import fallback


class PythonConfig(Language):
    """Configuration for the Python language."""

    def initial_dependencies(self) -> List[str]:
        return ["values.py"]

    def evaluator_dependencies(self) -> List[str]:
        return ["evaluation_utils.py"]

    def generation_callback(self, files: List[str]) -> CallbackResult:
        return (["python", "-m", "compileall", "-q", "-b", "."],
                [f.replace(".py", '.pyc') for f in files])

    def evaluator_generation_callback(self, files: List[str]) -> CallbackResult:
        return [], files

    def execution_command(self, cwd: str, file: str, dependencies: List[str],
                          arguments: List[str]) -> List[str]:
        return ["python", file, *arguments]

    def file_extension(self) -> str:
        return "py"

    def submission_name(self, plan: Plan) -> str:
        return "submission"

    def selector_name(self) -> str:
        return "selector"

    def context_name(self, tab_number: int, context_number: int) -> str:
        return f"context_{tab_number}_{context_number}"

    def conventionalise_function(self, function_name: str) -> str:
        return decamelize(function_name)

    def conventionalise_namespace(self, class_name: str) -> str:
        return depascalize(class_name)

    def context_dependencies_callback(self,
                                      context_name: str,
                                      dependencies: List[str]) -> List[str]:
        allowed = context_name + "."
        not_allowed = "context"
        return [x for x in dependencies
                if not x.startswith(not_allowed) or x.startswith(allowed)]

    def needs_selector(self):
        return False

    def process_compiler_output(
            self,
            stdout: str,
            stderr: str
    ) -> Optional[Tuple[List[Message], List[AnnotateCode]]]:
        if match := re.search(
                r".*: (?P<error>.+Error): (?P<message>.+) \(submission.py, "
                r"line (?P<line>\d+)\)",
                stdout):
            error = match.group('error')
            message = match.group('message')
            line = match.group('line')
            return [], [
                AnnotateCode(
                    row=int(line),
                    text=f"{error}: {message}",
                    type=Severity.ERROR
                )
            ]
        elif stuff := self._attempt_stacktrace(stdout):
            line, column, message = stuff
            return [], [
                AnnotateCode(
                    row=line,
                    column=column,
                    text=message,
                    type=Severity.ERROR
                )
            ]
        else:
            return None

    def _attempt_stacktrace(self, trace: str):
        # TODO: this only works with compiler traces.
        # Find message
        line_regex = fr'File "\.\{os.sep}submission\.py", line (?P<line>\d+)'
        if match := re.search(line_regex, trace):
            line = int(match.group("line")) - 1
        else:
            return None
        error_regex = r"(?P<error>.+Error): (?P<message>.+)"
        if match := re.search(error_regex, trace):
            message = match.group("error") + ": " + match.group("message")
        else:
            return None
        column_regex = r"^\s*\^\s*$"
        if match := re.search(column_regex, trace, re.MULTILINE):
            column_line = match.group(0)
            column = len(column_line) - len(column_line.lstrip())
        else:
            return None

        return line, column, message

    def run_linter(self, bundle: Bundle, submission: Path) \
            -> Tuple[List[Message], List[AnnotateCode]]:
        return python_linter.run_pylint(bundle, submission)

    def type_support_map(self) -> Mapping[AllTypes, TypeSupport]:
        return fallback(super().type_support_map(), {
            ant.DOUBLE_EXTENDED: TypeSupport.SUPPORTED,
            ant.FIXED_PRECISION: TypeSupport.SUPPORTED,
            ast.TUPLE:           TypeSupport.SUPPORTED,
            ast.LIST:            TypeSupport.SUPPORTED
        })
