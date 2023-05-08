from pathlib import Path
from typing import List, Tuple

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Message
from tested.languages.config import CallbackResult, Command, Config
from tested.languages.conventionalize import conventionalize_namespace, submission_file
from tested.languages.haskell.config import Haskell
from tested.languages.utils import haskell_cleanup_stacktrace


class RunHaskell(Haskell):
    def compilation(self, files: List[str]) -> CallbackResult:
        submission = submission_file(self, self.config.suite)
        main_file = list(filter(lambda x: x == submission, files))
        if main_file:
            return ["ghc", "-fno-code", main_file[0]], files
        else:
            return [], files

    def compiler_output(
        self, namespace: str, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        return (
            [],
            [],
            "",
            haskell_cleanup_stacktrace(
                stderr, self.with_extension(conventionalize_namespace(self, namespace))
            ),
        )

    def execution(
        self, config: Config, cwd: Path, file: str, arguments: List[str]
    ) -> Command:
        return ["runhaskell", file, *arguments]

    def filter_dependencies(
        self, bundle: Bundle, files: List[str], context_name: str
    ) -> List[str]:
        return files
