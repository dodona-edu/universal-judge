from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages.config import Command, Config, Language


class RunHaskell(Language):

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        return ["runhaskell", file, *arguments]

    def solution(self, solution: Path, bundle: Bundle):
        """Support implicit modules if needed."""
        if bundle.config.config_for().get("implicitModule", True):
            name = self.submission_name(bundle.plan)
            # noinspection PyTypeChecker
            with open(solution, "r") as file:
                contents = file.read()
            # noinspection PyTypeChecker
            with open(solution, "w") as file:
                result = f"module {name} where\n" + contents
                file.write(result)

    def filter_dependencies(self,
                            bundle: Bundle,
                            files: List[str],
                            context_name: str) -> List[str]:
        return files
