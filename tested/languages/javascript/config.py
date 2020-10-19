import logging
import re
from pathlib import Path
from typing import List, Tuple

from esprima import parseScript, error_handler

from tested.configs import Bundle
from tested.dodona import Message, AnnotateCode
from tested.languages.config import Command, Config, Language
from tested.languages.utils import cleanup_description

logger = logging.getLogger(__name__)


class JavaScript(Language):

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        return ['node', file, *arguments]

    # noinspection PyTypeChecker
    def solution(self, solution: Path, bundle: Bundle):
        try:
            with open(solution, "r") as file:
                contents = file.read()
            body = parseScript(contents).body
            functions = filter(
                lambda x: x.type in ('FunctionDeclaration', 'ClassDeclaration'),
                body
            )
            functions = filter(
                lambda x: x.id and x.id.type == 'Identifier',
                functions
            )
            functions = map(lambda x: x.id.name, functions)
            with open(solution, "a") as file:
                print("\nmodule.exports = {", ", ".join(functions), "};", file=file)
        except error_handler.Error:
            logger.debug("Failing to parse submission")

    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
        namespace = submission_file[:submission_file.rfind('.')]
        ref_not_found_regex = re.compile(rf"TypeError: {namespace}.([a-zA-Z0-9_]*) "
                                         r"is not a (function|constructor)")
        ref_not_found_replace = r'ReferenceError: \1 is not defined'

        context_file_regex = re.compile(r"context[0-9]+\.js")

        if isinstance(traceback, str):
            traceback = traceback.splitlines(True)

        skip_line, lines = False, []
        for line in traceback:

            line = line.strip('\n')

            if not line:
                continue

            # skip line if not a new File line is started
            if context_file_regex.search(line):
                skip_line = True
                continue
            elif skip_line and (line.startswith(' ') and 'at' not in line):
                continue

            # replace type error not found to reference error
            if ref_not_found_regex.search(line):
                line = ref_not_found_regex.sub(ref_not_found_replace, line)

            # replace references to local names
            if submission_file in line:
                line = re.sub(rf'\(.*{submission_file}(.*)\)', r'(<code>\1)', line)
            elif 'at ' in line:
                skip_line = True
                continue
            skip_line = False

            if not (reduce_all and line.startswith(' ')):
                lines.append(line + '\n')

        if len(lines) > 20:
            lines = lines[:19] + ['...\n'] + [lines[-1]]
        return "".join(lines)

    def cleanup_description(self, namespace: str, description: str) -> str:
        return cleanup_description(self, namespace, description)

    def stderr(self,
               bundle: Bundle,
               stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        # Identifier to separate testcase output
        identifier = f"--{bundle.secret}-- SEP"
        submission_file = self.with_extension(
            self.conventionalize_namespace(bundle.plan.namespace))
        # Assume stacktrace when line is equal the submission_file path with
        # line number
        line_start_with_submission_file = re.compile(
            rf'^(\\?([^\\/]*[\\/])*)({submission_file}):[0-9]+'
        )
        logger.debug(line_start_with_submission_file.pattern)
        cases = stderr.split(identifier)
        cleaned_cases = []
        # Process each case
        for case in cases:
            keep_until = 0
            case = case.splitlines(keepends=True)
            for index, line in enumerate(case):
                line = line.rstrip('\n')
                if not line_start_with_submission_file.match(line):
                    keep_until = index + 1
                else:
                    break
            keep_lines = "".join(case[:keep_until])
            stacktrace = "".join(case[keep_until:])
            stacktrace = self.cleanup_stacktrace(stacktrace, submission_file)
            cleaned_cases.append(f'{keep_lines}{stacktrace}')

        return [], [], identifier.join(cleaned_cases)
