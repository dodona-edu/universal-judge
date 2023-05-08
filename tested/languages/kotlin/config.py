import logging
import os
import re
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Optional, Tuple

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Message, Status
from tested.languages.config import CallbackResult, Command, Language, limit_output
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    conventionalize_namespace,
    submission_name,
)
from tested.languages.utils import jvm_cleanup_stacktrace, jvm_memory_limit, jvm_stderr
from tested.serialisation import FunctionCall, Statement, Value

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit

logger = logging.getLogger(__name__)


def get_executable(name):
    if os.name == "nt":
        return f"{name}.bat"
    return name


class Kotlin(Language):
    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        return {
            "namespace": "pascal_case",
            "function": "camel_case",
            "identifier": "camel_case",
            "global_identifier": "macro_case",
            "property": "camel_case",
            "class": "pascal_case",
        }

    def compilation(self, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return [
            get_executable("kotlinc"),
            f"-J-Xmx192M",
            "-nowarn",
            "-jvm-target",
            "11",
            "-cp",
            ".",
            *others,
        ], file_filter

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        limit = jvm_memory_limit(self.config)
        return [
            get_executable("kotlin"),
            f"-J-Xmx{limit}",
            "-cp",
            ".",
            Path(file).stem,
            *arguments,
        ]

    # noinspection PyTypeChecker
    def modify_solution(self, solution: Path):
        with open(solution, "r") as file:
            contents = file.read()
        # We use regex to find the main function.
        # First, check if we have a no-arg main function.
        # If so, replace it with a renamed main function that does have args.
        # Needed for main outside class
        no_args = re.compile(r"fun(\s+)main(\s*)\((\s*)\)")
        replacement = r"fun\1solutionMain\2(args: Array<String> = emptyArray()\3)"
        contents, nr = re.subn(no_args, replacement, contents, count=1)
        if nr == 0:
            # There was no main function without arguments. Now we try a main
            # function with arguments.
            with_args = re.compile(
                r"fun(\s+)main(\s*)\((\s*[^\s]*\s*:\s*" r"Array\s*<\s*String\s*>)"
            )
            replacement = r"fun\1solutionMain\2(\3"
            contents = re.sub(with_args, replacement, contents, count=1)
        with open(solution, "w") as file:
            file.write(contents)

    def linter(
        self, bundle: Bundle, submission: Path, remaining: float
    ) -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.kotlin import linter

        return linter.run_ktlint(bundle, submission, remaining)

    def find_main_file(
        self, files: List[str], name: str, precompilation_messages: List[str]
    ) -> Tuple[Optional[str], List[Message], Status, List[AnnotateCode]]:
        logger.debug("Finding %s in %s", name, files)
        main, msgs, status, ants = Language.find_main_file(
            self, files, name + "Kt", precompilation_messages
        )
        if status == Status.CORRECT:
            return main, msgs, status, ants
        else:
            return Language.find_main_file(self, files, name, precompilation_messages)

    def filter_dependencies(
        self, bundle: Bundle, files: List[Path], context_name: str
    ) -> List[str]:
        def filter_function(file: Path) -> bool:
            # We don't want files for contexts that are not the one we use.
            prefix = conventionalize_namespace(
                bundle.lang_config, bundle.lang_config.execution_prefix()
            )
            file = str(file)
            is_context = file.startswith(prefix)
            is_our_context = file.startswith(context_name + ".") or file.startswith(
                context_name + "$"
            )
            return not is_context or is_our_context

        return list(x for x in files if filter_function(x))

    def cleanup_stacktrace(
        self, traceback: str, submission_file: str, reduce_all=False
    ) -> str:
        return jvm_cleanup_stacktrace(traceback, submission_file, reduce_all)

    def compiler_output(
        self, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        return (
            [],
            [],
            limit_output(stdout),
            jvm_cleanup_stacktrace(stderr, submission_name(self)),
        )

    def stderr(self, stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        return jvm_stderr(self, stderr)

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.kotlin import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.kotlin import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: List[str]) -> str:
        from tested.languages.kotlin import generators

        return generators.convert_selector(contexts)

    def generate_check_function(self, name: str, function: FunctionCall) -> str:
        from tested.languages.kotlin import generators

        return generators.convert_check_function(function)

    def generate_encoder(self, values: List[Value]) -> str:
        from tested.languages.kotlin import generators

        return generators.convert_encoder(values)
