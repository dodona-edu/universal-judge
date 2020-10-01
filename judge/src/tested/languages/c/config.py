import re
from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages.config import CallbackResult, executable_name, Command, Config, Language


class C(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        main_file = files[-1]
        exec_file = Path(main_file).stem
        result = executable_name(exec_file)
        return (["gcc", "-std=c11", "-Wall", "evaluation_result.c", "values.c",
                 main_file, "-o", result], [result])

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        local_file = cwd / executable_name(Path(file).stem)
        return [str(local_file.absolute()), *arguments]

    # noinspection PyTypeChecker
    def solution(self, solution: Path, bundle: Bundle):
        with open(solution, "r") as file:
            contents = file.read()
        # We use regex to find the main function.
        # First, check if we have a no-arg main function.
        # If so, replace it with a renamed main function that does have args.
        no_args = re.compile(r"(int|void)\s+main\s*\(\s*\)\s*{")
        replacement = "int solution_main(int argc, char** argv){"
        contents, nr = re.subn(no_args, replacement, contents, count=1)
        if nr == 0:
            # There was no main function without arguments. Now we try a main
            # function with arguments.
            with_args = re.compile(r"(int|void)\s+main\s*\(\s*int")
            replacement = "int solution_main(int"
            contents = re.sub(with_args, replacement, contents, count=1)
        with open(solution, "w") as file:
            header = "#pragma once\n\n"
            file.write(header + contents)
