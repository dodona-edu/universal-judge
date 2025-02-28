from pathlib import Path

from tested.datatypes import AllTypes
from tested.features import Construct, TypeSupport
from tested.languages.c.config import C
from tested.languages.conventionalize import Conventionable, NamingConventions
from tested.languages.cpp.generators import CPPGenerator
from tested.languages.language import CallbackResult
from tested.languages.utils import executable_name


class CPP(C):
    def initial_dependencies(self) -> list[str]:
        return [
            "values.h",
            "values.cpp",
            "values.tpp",
            "evaluation_result.h",
            "evaluation_result.cpp",
        ]

    def file_extension(self) -> str:
        return "cpp"

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {
            "identifier": "camel_case",
            "property": "camel_case",
            "class": "pascal_case",
            "global_identifier": "macro_case",
        }

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.GLOBAL_VARIABLES,
            Construct.OBJECTS,
            Construct.HETEROGENEOUS_COLLECTIONS,
            Construct.DEFAULT_PARAMETERS,
            Construct.HETEROGENEOUS_ARGUMENTS,
        }

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return super().datatype_support() | {  # type: ignore
            "sequence": "supported",
            "set": "supported",
            "map": "supported",
            "dictionary": "supported",
            "object": "reduced",
            "array": "supported",
            "list": "supported",
            "tuple": "supported",
        }

    def compilation(self, files: list[str]) -> CallbackResult:
        main_file = files[-1]
        exec_file = Path(main_file).stem
        result = executable_name(exec_file)
        assert self.config
        return (
            [
                "g++",
                "-std=c++17",
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
