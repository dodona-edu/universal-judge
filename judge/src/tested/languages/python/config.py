import os
import re
from pathlib import Path
from typing import List, Tuple

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Severity, Message
from tested.languages.config import Language, CallbackResult, Command
from tested.languages.python import linter


class PythonConfig(Language):

    def compilation(self, files: List[str]) -> CallbackResult:

        def file_filter(file: Path, context: str) -> bool:
            # We only allow pyc files
            is_pyc = file.suffix == ".pyc"
            # We don't want files for contexts that are not the one we use.
            is_context = file.name.startswith("context")
            is_our_context = file.name.startswith(context + ".")
            return is_pyc and (not is_context or is_our_context)

        return ["python", "-m", "compileall", "-q", "-b", "."], file_filter

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        return ["python", "-u", file, *arguments]

    def compiler_output(self, stdout: str, stderr: str) \
            -> Tuple[List[Message], List[AnnotateCode], str, str]:
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
            ], "", stderr
        elif stuff := self._attempt_stacktrace(stdout):
            line, column, message = stuff
            return [], [
                AnnotateCode(
                    row=line,
                    column=column,
                    text=message,
                    type=Severity.ERROR
                )
            ], "", stderr
        else:
            return [], [], stdout, stderr

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

    def linter(self, bundle: Bundle, submission: Path, remaining: float) \
            -> Tuple[List[Message], List[AnnotateCode]]:
        return linter.run_pylint(bundle, submission, remaining)
