import logging
import os
import re
from pathlib import Path
from typing import List, Tuple, Optional

from tested.configs import Bundle
from tested.dodona import Message, Status, AnnotateCode, ExtendedMessage, Permission
from tested.languages.config import CallbackResult, Command, Config, Language, \
    limit_output
from tested.languages.utils import jvm_memory_limit, jvm_cleanup_stacktrace, \
    jvm_stderr

logger = logging.getLogger(__name__)


def get_executable(name):
    if os.name == 'nt':
        return f"{name}.bat"
    return name


class Kotlin(Language):

    def compilation(self, bundle: Bundle, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return [get_executable("kotlinc"), f"-J-Xmx192M", "-nowarn", "-jvm-target",
                "11", "-cp", ".", *others], file_filter

    def execution(self, config: Config, cwd: Path, file: str,
                  arguments: List[str]) -> Command:
        limit = jvm_memory_limit(config)
        return [get_executable("kotlin"), f"-J-Xmx{limit}", "-cp", ".",
                Path(file).stem, *arguments]

    # noinspection PyTypeChecker
    def solution(self, solution: Path, bundle: Bundle):
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
            with_args = re.compile(r"fun(\s+)main(\s*)\((\s*[^\s]*\s*:\s*"
                                   r"Array\s*<\s*String\s*>)")
            replacement = r"fun\1solutionMain\2(\3"
            contents = re.sub(with_args, replacement, contents, count=1)
        with open(solution, "w") as file:
            file.write(contents)

    def linter(self, bundle: Bundle, submission: Path, remaining: float) \
            -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.kotlin import linter
        return linter.run_ktlint(bundle, submission, remaining)

    def find_main_file(self, files: List[str], name: str) \
            -> Tuple[Optional[str], List[Message], Status, List[AnnotateCode]]:
        logger.debug("Finding %s in %s", name, files)
        main, msgs, status, ants = Language.find_main_file(self, files, name + 'Kt')
        if status == Status.CORRECT:
            return main, msgs, status, ants
        else:
            return Language.find_main_file(self, files, name)

    def filter_dependencies(self,
                            bundle: Bundle,
                            files: List[str],
                            context_name: str) -> List[str]:
        def filter_function(file: str) -> bool:
            # We don't want files for contexts that are not the one we use.
            prefix = bundle.lang_config.conventionalize_namespace(
                bundle.lang_config.execution_prefix()
            )
            is_context = file.startswith(prefix)
            is_our_context = (file.startswith(context_name + ".") or
                              file.startswith(context_name + "$"))
            return not is_context or is_our_context

        return list(x for x in files if filter_function(x))

    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
        return jvm_cleanup_stacktrace(traceback, submission_file, reduce_all)

    def compiler_output(
            self, namespace: str, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        return [], [], limit_output(stdout), jvm_cleanup_stacktrace(
            stderr,
            self.with_extension(self.conventionalize_namespace(namespace))
        )

    def stderr(self,
               bundle: Bundle,
               stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        return jvm_stderr(self, bundle, stderr)
