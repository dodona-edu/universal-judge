from typing import List, Optional

from features import Features
from runners.config import CallbackResult
from runners.languages.java import JavaConfig
from testplan import Context

EXECUTE_SCRIPT = "exit.jsh"


class JshellConfig(JavaConfig):
    """
    Configuration for the Java language in script mode.
    This supports most features of the regular Java runner, except you can pass it a jShell script.
    """

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        pass

    def execution_command(self, files: List[str]) -> List[str]:
        # We modify the order of the files to have what we want: separate jar, java and jsh files.
        jars = []
        java = []
        jsh = []
        for file in files:
            if file.endswith(".jar"):
                jars.append("--class-path")
                jars.append(file)
            elif file.endswith(".java"):
                java.append(file)
            elif file.endswith(EXECUTE_SCRIPT) or file.endswith("script.jsh"):  # Skip this
                pass
            else:
                assert file.endswith(".jsh")
                jsh.append(file)

        return ["jshell"] + jars + java + jsh + [EXECUTE_SCRIPT]

    def file_extension(self) -> str:
        return "jsh"

    def compilation_callback(self, files: List[str]) -> CallbackResult:
        return [], files

    def submission_name(self, context: Context) -> Optional[str]:
        return "script"

    def template_folders(self, programming_language: str) -> List[str]:
        return super().template_folders(programming_language) + ["java"]

    def initial_dependencies(self) -> List[str]:
        return super().initial_dependencies() + ["exit.jsh"]

    def template_extensions(self) -> List[str]:
        return super().template_extensions() + ["java"]

    def supported_features(self) -> Features:
        return (
                Features.OBJECTS | Features.EXCEPTIONS | Features.FUNCTION_CALL | Features.ASSIGNMENT |
                Features.LISTS | Features.SETS | Features.MAPS |
                Features.INTEGERS | Features.RATIONALS | Features.STRINGS | Features.BOOLEANS | Features.NULL
        )
