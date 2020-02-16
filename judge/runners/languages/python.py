from humps import decamelize, depascalize
from typing import List

from runners.config import CallbackResult, LanguageConfig
from testplan import Plan, Context


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

    def execution_command(self, file: str, dependencies: List[str],
                          arguments: List[str]) -> List[str]:
        return ["python", file, *arguments]

    def file_extension(self) -> str:
        return "py"

    def submission_name(self, plan: Plan) -> str:
        return "submission"

    def selector_name(self) -> str:
        return "selector"

    def context_name(self, number: int) -> str:
        return f"context_{number}"

    def conventionalise(self, function_name: str) -> str:
        return decamelize(function_name)

    def conventionalise_object(self, class_name: str) -> str:
        return depascalize(class_name)
