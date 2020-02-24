from typing import List

from humps import decamelize, depascalize

from runners.config import CallbackResult, LanguageConfig
from testplan import Plan


class PythonConfig(LanguageConfig):
    """Configuration for the Python language."""

    def initial_dependencies(self) -> List[str]:
        return ["values.py"]

    def evaluator_dependencies(self) -> List[str]:
        return ["evaluation_utils.py"]

    def generation_callback(self, files: List[str]) -> CallbackResult:
        return (["python", "-m", "compileall", "-b", "."],
                [f.replace(".py", '.pyc') for f in files])

    def evaluator_generation_callback(self, files: List[str]) -> CallbackResult:
        return [], files

    def execution_command(self, cwd: str, file: str, dependencies: List[str],
                          arguments: List[str]) -> List[str]:
        return ["python", file, *arguments]

    def file_extension(self) -> str:
        return "py"

    def submission_name(self, plan: Plan) -> str:
        return "submission"

    def selector_name(self) -> str:
        return "selector"

    def context_name(self, tab_number: int, context_number: int) -> str:
        return f"context_{tab_number}_{context_number}"

    def conventionalise(self, function_name: str) -> str:
        return decamelize(function_name)

    def conventionalise_object(self, class_name: str) -> str:
        return depascalize(class_name)

    def context_dependencies_callback(self,
                                      context_name: str,
                                      dependencies: List[str]) -> List[str]:
        allowed = context_name + "."
        not_allowed = "context"
        return [x for x in dependencies
                if not x.startswith(not_allowed) or x.startswith(allowed)]

    def needs_selector(self):
        return False
