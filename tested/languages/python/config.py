import os
import re
from pathlib import Path
from typing import List, Tuple, Optional

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Severity, Message, ExtendedMessage, \
    Permission
from tested.languages.config import Language, CallbackResult, Command, Config, \
    trace_to_html


def _executable():
    if os.name == 'nt':
        return 'python'
    else:
        return 'python3'


class Python(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        result = [x.replace(".py", ".pyc") for x in files]
        return [_executable(), "-m", "compileall", "-q", "-b", "."], result

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        return [_executable(), "-u", file, *arguments]

    def compiler_output(self, namespace: str, stdout: str, stderr: str) \
            -> Tuple[List[Message], List[AnnotateCode], str, str]:
        if match := re.search(
                rf".*: (?P<error>.+Error): (?P<message>.+) \({namespace}.py, "
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
            ], stdout, stderr
        elif stuff := self._attempt_stacktrace(namespace, stdout):
            line, column, message = stuff
            return [], [
                AnnotateCode(
                    row=line,
                    column=column,
                    text=message,
                    type=Severity.ERROR
                )
            ], stdout, stderr
        else:
            return [], [], stdout, stderr

    def _attempt_stacktrace(self, namespace: str, trace: str):
        # TODO: this only works with compiler traces.
        # Find message
        line_regex = fr'File "\.\{os.sep}{namespace}\.py", line (?P<line>\d+)'
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
        # Import locally to prevent errors.
        from tested.languages.python import linter
        return linter.run_pylint(bundle, submission, remaining)

    # Idea and original code: dodona/judge-pythia
    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
        context_file_regex = re.compile(r"context_[0-9]+_[0-9]+\.py")

        if isinstance(traceback, str):
            traceback = traceback.splitlines(True)

        skip_line, lines = False, []
        for line in traceback:

            line = line.strip('\n')

            if not line:
                continue

            if line.startswith('During handling of the above exception, another '
                               'exception occurred:'):
                lines = []
                continue

            # skip line if not a new File line is started
            if context_file_regex.search(line):
                skip_line = True
                continue
            elif skip_line and (not line.startswith(' ') or 'File' in line):
                skip_line = False
            elif skip_line:
                continue

            # replace references to local names
            if f'File "./{submission_file}"' in line:
                line = line.replace(f'File "./{submission_file}"', 'File "<code>"')
            elif 'File "<string>"' in line:
                line = line.replace('File "<string>"', 'File "<code>"')
            elif 'File "<doctest>"' in line:
                continue
            elif 'File "' in line:
                skip_line = True
                continue
            skip_line = False

            # replace references to modules
            if ', in <module>' in line:
                line = line.replace(', in <module>', '')

            if not (reduce_all and line.startswith(' ')):
                lines.append(line + '\n')

        if len(lines) > 20:
            lines = lines[:19] + ['...\n'] + [lines[-1]]
        return "".join(lines)

    def clean_stacktrace_to_message(self, stacktrace: str) -> Optional[Message]:
        if stacktrace:
            return trace_to_html(stacktrace,
                                 r'File &quot;&lt;code&gt;&quot;, line ([0-9]+)',
                                 r'File <a href="#" class="tab-link" '
                                 r'data-tab="code" data-line="\1">'
                                 r'&quot;&lt;code&gt;&quot;, line \1</a>')
        else:
            return None
