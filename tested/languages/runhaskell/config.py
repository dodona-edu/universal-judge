from pathlib import Path

from tested.languages.conventionalize import submission_file
from tested.languages.haskell.config import Haskell
from tested.languages.language import CallbackResult, Command


class RunHaskell(Haskell):
    def compilation(self, files: list[str], directory: Path) -> CallbackResult:
        submission = submission_file(self)
        main_file = list(filter(lambda x: x == submission, files))
        if main_file:
            return ["ghc", "-fno-code", main_file[0]], files
        else:
            return [], files

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        return ["runhaskell", file, *arguments]

    def filter_dependencies(self, files: list[Path], context_name: str) -> list[Path]:
        return files

    def path_to_dependencies(self) -> list[Path]:
        """
        Construct the paths to the folder containing the additional dependencies
        needed for a programming language.

        :return: A list of template folders.
        """
        assert self.config
        return [
            self.config.dodona.judge / "tested" / "languages" / "haskell" / "templates"
        ]
