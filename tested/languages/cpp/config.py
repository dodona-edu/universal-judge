from pathlib import Path
from tested.features import Construct

from tested.languages.c.config import C
from tested.languages.conventionalize import Conventionable, NamingConventions
from tested.languages.cpp.generators import CPPGenerator
from tested.languages.language import CallbackResult
from tested.languages.utils import executable_name


class CPP(C):
    def initial_dependencies(self) -> list[str]:
        return ["values.h", "values.cpp", "evaluation_result.h", "evaluation_result.cpp"]

    def file_extension(self) -> str:
        return "cpp"
    
    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {
            "identifier": "pascal_case",
            "property": "pascal_case",
            "class": "pascal_case",
            "global_identifier": "macro_case",
        }

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.GLOBAL_VARIABLES,
            Construct.OBJECTS,
        }

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

    def generator(self) -> CPPGenerator:
        return CPPGenerator(self.file_extension())
