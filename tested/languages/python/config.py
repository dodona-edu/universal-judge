import logging
import os
import re
from pathlib import Path
from typing import List, Tuple, Optional

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Severity, Message
from tested.languages.config import Language, CallbackResult, Command, Config, \
    trace_to_html

logger = logging.getLogger(__name__)


def _executable():
    if os.name == 'nt':
        return 'python'
    else:
        return 'python3'


class Python(Language):

    def get_string_quote(self):
        return '\''

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        result = [x.replace(".py", ".pyc") for x in files]
        return [_executable(), "-W", "ignore", "-m", "compileall", "-q", "-b",
                "."], result

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        return [_executable(), "-u", file, *arguments]

    def compiler_output(self, namespace: str, stdout: str, stderr: str) \
            -> Tuple[List[Message], List[AnnotateCode], str, str]:
        stdout = self.cleanup_stacktrace(stdout, self.with_extension(namespace))
        if match := re.search(r".*: (.+Error): (.+) \(<code>, line (\d+)\)",
                              stdout):
            error = match.group(1)
            message = match.group(2)
            line = match.group(3)
            return [], [
                AnnotateCode(
                    row=int(line),
                    text=f"{error}: {message}",
                    type=Severity.ERROR
                )
            ], stdout, stderr
        elif stuff := self._attempt_stacktrace(stdout):
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

    def _attempt_stacktrace(self, trace: str):
        # TODO: this only works with compiler traces.
        # Find message
        line_regex = fr'File "<code>", line (?P<line>\d+)'
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
        file_line_regex = re.compile(rf'\({submission_file}, line (\d+)\)')

        if isinstance(traceback, str):
            traceback = traceback.splitlines(True)

        skip_line, lines = False, []
        for line in traceback:

            line = line.strip('\n')
            logger.debug(line)

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
            elif line.startswith("*** Error compiling"):
                line = line.replace(f'./{submission_file}', '<code>')
            elif file_line_regex.search(line):
                line = file_line_regex.sub(r'(<code>, line \1)', line)
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
            trace = trace_to_html(stacktrace,
                                  r'File &quot;&lt;code&gt;&quot;, line ([0-9]+)',
                                  r'File <a href="#" class="tab-link" '
                                  r'data-tab="code" data-line="\1">'
                                  r'&quot;&lt;code&gt;&quot;, line \1</a>')
            additional_regex = re.compile(r'\(&lt;code&gt;, line (\d+)\)')
            additional_sub = r'(<a href="#" class="tab-link" data-tab="code" ' \
                             r'data-line="\1">&lt;code&gt;, line \1</a>)'
            trace.description = additional_regex.sub(additional_sub,
                                                     trace.description)
            logger.debug(trace.description)
            return trace
        else:
            return None
