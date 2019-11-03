from pathlib import Path
from typing import List

from runners.common import LanguageConfig
from testplan import Plan, Context


class PythonConfig(LanguageConfig):
    """Configuration for the Python language."""

    def supports_top_level_functions(self) -> bool:
        return True

    def needs_compilation(self) -> bool:
        return True

    def compilation_command(self, files: List[str]) -> List[str]:
        return ["python", "-m", "py_compile", *files]

    def execution_command(self, context_id: str) -> List[str]:
        context = f"{self.context_name(context_id)}.{self.file_extension()}"
        return ["python", str(context)]

    def file_extension(self) -> str:
        return "py"

    def submission_name(self, context_id: str, context: Context) -> str:
        return f"submission_{context_id}"

    def context_name(self, context_id: str) -> str:
        return f"context_{context_id}"

    def additional_files(self) -> List[str]:
        return ["values.py"]

    def needs_main(self):
        return False
