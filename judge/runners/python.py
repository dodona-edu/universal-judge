from typing import List

from humps import decamelize

from runners.runner import LanguageConfig
from testplan import Context


class PythonConfig(LanguageConfig):
    """Configuration for the Python language."""

    def value_writer(self, name):
        return f"def {name}(value):\n    send(value)"

    def supports_top_level_functions(self) -> bool:
        return True

    def needs_compilation(self) -> bool:
        return True

    def compilation_command(self, files: List[str]) -> List[str]:
        return ["python", "-m", "py_compile", *files]

    def execution_command(self, context_id: str) -> List[str]:
        context = f"{self.context_name(context_id)}.{self.file_extension()}"
        return ["python", context]

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        file = f"{self.evaluator_name('eval')}.{self.file_extension()}"
        return ["python", file]

    def file_extension(self) -> str:
        return "py"

    def submission_name(self, context_id: str, context: Context) -> str:
        return f"submission_{context_id}"

    def context_name(self, context_id: str) -> str:
        return f"context_{context_id}"

    def evaluator_name(self, context_id: str) -> str:
        return f"evaluator_{context_id}"

    def additional_files(self) -> List[str]:
        return ["values.py"]

    def conventionalise(self, function_name: str) -> str:
        return decamelize(function_name)
