from pathlib import Path
from typing import List

from tested.languages.config import CallbackResult, Command
from tested.languages.conventionalize import submission_file
from tested.languages.haskell.config import Haskell


class RunHaskell(Haskell):
    def compilation(self, files: List[str]) -> CallbackResult:
        submission = submission_file(self)
        main_file = list(filter(lambda x: x == submission, files))
        if main_file:
            return ["ghc", "-fno-code", main_file[0]], files
        else:
            return [], files

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        return ["runhaskell", file, *arguments]

    def filter_dependencies(self, files: List[str], context_name: str) -> List[str]:
        return files

    def path_to_dependencies(self) -> List[Path]:
        """
        Construct the paths to the folder containing the additional dependencies
        needed for a programming language.

        :return: A list of template folders.
        """
        assert self.config
        return [
            self.config.dodona.judge / "tested" / "languages" / "haskell" / "templates"
        ]
