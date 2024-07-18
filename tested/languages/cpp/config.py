from pathlib import Path
import re

from tested.languages.c.config import C
from tested.languages.language import CallbackResult
from tested.languages.utils import executable_name


class CPP(C):
    def initial_dependencies(self) -> list[str]:
        return ["values.h", "values.cpp", "evaluation_result.h", "evaluation_result.cpp"]

    def file_extension(self) -> str:
        return "cpp"

    def compilation(self, files: list[str]) -> CallbackResult:
        main_file = files[-1]
        exec_file = Path(main_file).stem
        result = executable_name(exec_file)
        return (
            [
                "g++",
                "-std=c++11",
                "-Wall",
                "-O3" if self.config.options.compiler_optimizations else "-O0",
                "evaluation_result.cpp",
                "values.cpp",
                main_file,
                "-o",
                result,
            ],
            [result],
        )

