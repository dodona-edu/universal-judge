import logging
import re
from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages.config import CallbackResult, Command, Config, Language
from tested.languages.utils import memory_limit_jvm

logger = logging.getLogger(__name__)


class Kotlin(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return ["kotlinc", "-nowarn", "-jvm-target", "11", "-cp", ".", *others], file_filter

    def execution(self, config: Config, cwd: Path, file: str,
                  arguments: List[str]) -> Command:
        limit = memory_limit_jvm(config)
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
        name += "Kt"
        logger.debug("Finding %s in %s", name, files)
        return [x for x in files if x.startswith(name)][0]
