from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages import Language
from tested.languages.config import CallbackResult, executable_name, Command


class CConfig(Language):

    def compilation(self, files: List[str]) -> CallbackResult:
        main_file = files[-1]
        exec_file = Path(main_file).stem
        result = executable_name(exec_file)
        return (["gcc", "-std=c11", "-Wall", "evaluation_result.c", "values.c",
                 main_file, "-o", result], [result])

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        local_file = cwd / executable_name(Path(file).stem)
        return [str(local_file.absolute()), *arguments]

    # noinspection PyTypeChecker
    def solution(self, solution: Path, bundle: Bundle):
        with open(solution, "r") as file:
            contents = file.read()
        with open(solution, "w") as file:
            header = "#pragma once\n\n"
            result = header + contents.replace("main", "solution_main")
            file.write(result)

