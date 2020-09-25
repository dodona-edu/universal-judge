import re
from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages.config import CallbackResult, Command, Config, Language


def _memory_limit(config: Config) -> int:
    """
    Get the memory limit in bytes. Java requires this to be a multiple of 1024.
    See https://docs.oracle.com/en/java/javase/14/docs/specs/man/java.html
    """
    limit = int(config.memory_limit)
    limit = (limit // 1024) * 1024
    return limit


class Kotlin(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return ["kotlinc", "-nowarn", "-jvm-target", "11", "-cp", ".", *others], file_filter

    def execution(self, config: Config, cwd: Path, file: str,
                  arguments: List[str]) -> Command:
        limit = _memory_limit(config)
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
            with_args = re.compile(r"fun\s+main\s*\(\s*([^\s])*\s*:\s*"
                                   r"Array\s*<\s*String\s*>")
            replacement = "fun solutionMain(\\1: Array<String>"
            contents = re.sub(with_args, replacement, contents, count=1)
        with open(solution, "w") as file:
            file.write(contents)
