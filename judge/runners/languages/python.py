from humps import decamelize
from typing import List

from runners.config import CallbackResult
from runners.runner import LanguageConfig
from testplan import Plan


class PythonConfig(LanguageConfig):
    """Configuration for the Python language."""

    def initial_dependencies(self) -> List[str]:
        return ["values.py"]

    def evaluator_dependencies(self) -> List[str]:
        return ["evaluation_utils.py"]

    def value_writer(self, name):
        return f"def {name}(value): send(value)"

    def exception_writer(self, name):
        return f"def {name}(exception): send_exception(exception)"

    def generation_callback(self, files: List[str]) -> CallbackResult:
        return ["python", "-m", "compileall", "-b", "."], [f.replace(".py", '.pyc') for f in files]

    def evaluator_generation_callback(self, files: List[str]) -> CallbackResult:
        return [], files

    def execution_command(self, file: str, dependencies: List[str], arguments: List[str]) -> List[str]:
        return ["python", file, *arguments]

    def file_extension(self) -> str:
        return "py"

    def submission_name(self, plan: Plan) -> str:
        return f"submission"

    def context_name(self) -> str:
        return f"context"

    def evaluator_name(self) -> str:
        return f"evaluator"

    def conventionalise(self, function_name: str) -> str:
        return decamelize(function_name)

    def conventionalise_object(self, class_name: str) -> str:
        return decamelize(class_name)
