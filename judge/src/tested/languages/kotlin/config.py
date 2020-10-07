import logging
import re
from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages.config import CallbackResult, Command, Config, Language
from tested.languages.utils import jvm_memory_limit

logger = logging.getLogger(__name__)


class Kotlin(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return ["kotlinc", "-nowarn", "-jvm-target", "11", "-cp", ".",
                *others], file_filter

    def execution(self, config: Config, cwd: Path, file: str,
                  arguments: List[str]) -> Command:
        limit = jvm_memory_limit(config)
        return ["kotlin", f"-J-Xmx{limit}", "-cp", ".", Path(file).stem, *arguments]

    # noinspection PyTypeChecker
    def solution(self, solution: Path, bundle: Bundle):
        with open(solution, "r") as file:
            contents = file.read()
        # We use regex to find the main function.
        # First, check if we have a no-arg main function.
        # If so, replace it with a renamed main function that does have args.
        # Needed for main outside class
        no_args = re.compile(r"fun\s+main\s*\(\s*\)\s*{")
        replacement = "fun solutionMain(args: Array<String> = emptyArray()){"
        contents, nr = re.subn(no_args, replacement, contents, count=1)
        if nr == 0:
            # There was no main function without arguments. Now we try a main
            # function with arguments.
            with_args = re.compile(r"fun\s+main\s*\(\s*([^\s]*)\s*:\s*"
                                   r"Array\s*<\s*String\s*>")
            replacement = "fun solutionMain(\\1: Array<String>"
            contents = re.sub(with_args, replacement, contents, count=1)
        with open(solution, "w") as file:
            file.write(contents)

    def find_main_file(self, files: List[str], name: str) -> str:
        logger.debug("Finding %s in %s", name, files)
        possible_main = [x for x in files if x.startswith(name + "Kt")]
        if possible_main:
            return possible_main[0]
        else:
            return [x for x in files if x.startswith(name)][0]

    def filter_dependencies(self,
                            bundle: Bundle,
                            files: List[str],
                            context_name: str) -> List[str]:
        def filter_function(file: str) -> bool:
            # We don't want files for contexts that are not the one we use.
            prefix = bundle.lang_config.conventionalize_namespace(
                bundle.lang_config.context_prefix()
            )
            is_context = file.startswith(prefix)
            is_our_context = (file.startswith(context_name + ".") or
                              file.startswith(context_name + "$"))
            return not is_context or is_our_context

        return list(x for x in files if filter_function(x))
