import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Tuple

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Message
from tested.languages.config import (
    CallbackResult,
    Command,
    Config,
    Language,
    limit_output,
)
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    conventionalize_namespace,
    submission_file,
)
from tested.languages.utils import cleanup_description
from tested.serialisation import FunctionCall, Statement, Value

if TYPE_CHECKING:
    from tested.languages.generator import PreparedExecutionUnit

logger = logging.getLogger(__name__)


class JavaScript(Language):
    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        return {
            "namespace": "camel_case",
            "function": "camel_case",
            "identifier": "camel_case",
            "global_identifier": "macro_case",
            "property": "camel_case",
            "class": "pascal_case",
        }

    def compilation(self, bundle: Bundle, files: List[str]) -> CallbackResult:
        submission = submission_file(self, bundle.suite)
        main_file = list(filter(lambda x: x == submission, files))
        if main_file:
            return ["node", "--check", main_file[0]], files
        else:
            return [], files

    def compiler_output(
        self, namespace: str, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        return (
            [],
            [],
            limit_output(stdout),
            self.cleanup_stacktrace(
                stderr, self.with_extension(conventionalize_namespace(self, namespace))
            ),
        )

    def execution(
        self, config: Config, cwd: Path, file: str, arguments: List[str]
    ) -> Command:
        return ["node", file, *arguments]

    # noinspection PyTypeChecker
    def solution(self, solution: Path, bundle: Bundle):
        # import local to prevent errors
        from tested.judge.utils import run_command

        parse_file = Path(__file__).parent / "parseAst.js"
        try:
            output = run_command(
                solution.parent,
                timeout=None,
                command=["node", parse_file, solution.absolute()],
            )
            namings = output.stdout.strip()
            with open(solution, "a") as file:
                print(f"\nmodule.exports = {{{namings}}};", file=file)
        except TimeoutError:
            pass

    def linter(
        self, bundle: Bundle, submission: Path, remaining: float
    ) -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.javascript import linter

        return linter.run_eslint(bundle, submission, remaining)

    def cleanup_stacktrace(
        self, traceback: str, submission_file: str, reduce_all=False
    ) -> str:
        namespace = submission_file[: submission_file.rfind(".")]
        line_start_with_submission_file = re.compile(
            rf"^(\\?([^\\/]*[\\/])*)({submission_file}:)(?P<loc>[0-9]+)"
        )
        ref_not_found_regex = re.compile(
            rf"TypeError: {namespace}.([a-zA-Z0-9_]*) "
            r"is not a (function|constructor)"
        )
        ref_not_found_replace = r"ReferenceError: \1 is not defined"

        context_file_regex = re.compile(r"context[0-9]+\.js")
        submission_file_regex = re.compile(rf"\(.*{submission_file}(.*)\)")
        submission_file_replace = r"(<code>\1)"
        at_code_regex = re.compile(r"at .* \((<code>:[0-9]+:[0-9]+)\)")
        at_code_replace = r"at \1"

        if isinstance(traceback, str):
            traceback = traceback.splitlines(True)

        skip_line, lines = False, []
        for line in traceback:
            line = line.strip("\n")

            if not line:
                continue

            # skip line if not a new File line is started
            if context_file_regex.search(line):
                skip_line = True
                continue
            elif skip_line and (line.startswith(" ") and "at" not in line):
                continue

            # replace type error not found to reference error
            if ref_not_found_regex.search(line):
                line = ref_not_found_regex.sub(ref_not_found_replace, line)

            # replace references to local names
            if submission_file in line:
                line = submission_file_regex.sub(submission_file_replace, line)
            elif "at " in line:
                skip_line = True
                continue
            skip_line = False

            # Replace submission file line
            match = line_start_with_submission_file.match(line)
            if match:
                line = f"<code>:{match.group('loc')}"

            # Remove textual location information
            line = at_code_regex.sub(at_code_replace, line)

            if not (reduce_all and line.startswith(" ")):
                lines.append(line + "\n")

        if len(lines) > 20:
            lines = lines[:19] + ["...\n"] + [lines[-1]]
        return "".join(lines)

    def cleanup_description(self, namespace: str, description: str) -> str:
        description = cleanup_description(self, namespace, description)
        await_regex = re.compile(r"await\s+")
        return await_regex.sub("", description)

    def clean_exception_message(self, message: str, namespace: str) -> str:
        return message.replace(f"{namespace}.", "", 1)

    def stderr(
        self, bundle: Bundle, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str]:
        # Identifier to separate testcase output
        identifier = f"--{bundle.testcase_separator_secret}-- SEP"
        context_identifier = f"--{bundle.context_separator_secret}-- SEP"
        submission_file = self.with_extension(
            conventionalize_namespace(self, bundle.suite.namespace)
        )
        # Assume stacktrace when line is equal the submission_file path with
        # line number
        line_start_with_submission_file = re.compile(
            rf"^(\\?([^\\/]*[\\/])*)({submission_file}):[0-9]+"
        )
        contexts = stderr.split(context_identifier)
        cleaned_contexts = []
        for context in contexts:
            cases = context.split(identifier)
            cleaned_cases = []
            # Process each case
            for case in cases:
                keep_until = 0
                case = case.splitlines(keepends=True)
                for index, line in enumerate(case):
                    line = line.rstrip("\n")
                    if not line_start_with_submission_file.match(line):
                        keep_until = index + 1
                    else:
                        break
                keep_lines = "".join(case[:keep_until])
                stacktrace = "".join(case[keep_until:])
                stacktrace = self.cleanup_stacktrace(stacktrace, submission_file)
                cleaned_cases.append(f"{keep_lines}{stacktrace}")
            cleaned_contexts.append(identifier.join(cleaned_cases))

        return [], [], context_identifier.join(cleaned_contexts)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.javascript import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.javascript import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_check_function(self, name: str, function: FunctionCall) -> str:
        from tested.languages.javascript import generators

        return generators.convert_check_function(name, function)

    def generate_encoder(self, values: List[Value]) -> str:
        from tested.languages.javascript import generators

        return generators.convert_encoder(values)
