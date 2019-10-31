from pathlib import Path
from typing import List

from runners.common import LanguageConfig
from testplan import Plan


class PythonConfig(LanguageConfig):

    def supports_top_level_functions(self) -> bool:
        return True

    def needs_compilation(self) -> bool:
        return False

    # TODO: fix broken path when executing
    def execution_command(self, context_id: str, path: Path) -> List[str]:
        context = path / f"{self.context_name(context_id)}.{self.file_extension()}"
        return ["python", str(context)]

    def file_extension(self) -> str:
        return "py"

    def submission_name(self, plan: Plan) -> str:
        return "submission"

    def context_name(self, context_id: str) -> str:
        return f"context_{context_id}"

    def additional_files(self) -> List[str]:
        return ["values.py"]

    def needs_main(self):
        return False
