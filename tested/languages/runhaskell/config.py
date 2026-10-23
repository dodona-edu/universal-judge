from pathlib import Path

from tested.languages.conventionalize import submission_file
from tested.languages.haskell.config import Haskell, precompiled_helpers
from tested.languages.language import CallbackResult, Command


class RunHaskell(Haskell):
    def _precompiled_helpers(self) -> Path | None:
        # GHCi loads them instead of interpreting them, as long as they are dynamic.
        return precompiled_helpers("dynamic")

    def compilation(self, files: list[str]) -> CallbackResult:
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
