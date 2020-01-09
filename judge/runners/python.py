from humps import decamelize
from typing import List

from runners.config import CallbackResult
from runners.runner import LanguageConfig
from testplan import Plan


class PythonConfig(LanguageConfig):
    """Configuration for the Python language."""

    def initial_dependencies(self) -> List[str]:
        return ["values.py"]

    def value_writer(self, name):
        return f"def {name}(value): send(value)"

    def exception_writer(self, name):
        return f"def {name}(exception): send_exception(exception)"

    def pre_compilation_callback(self, files: List[str]) -> CallbackResult:
        return ["python", "-m", "compileall", "-b"], [f.replace(".py", '.pyc') for f in files]

    def execution_command(self, files: List[str]) -> List[str]:
        main = self._get_main_file(files)
        return ["python", main]

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        return ["python", f"{self.evaluator_name()}.{self.file_extension()}"]

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
