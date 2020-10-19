from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages.config import Command, Config, Language
from tested.languages.utils import haskell_solution, cleanup_description, \
    haskell_cleanup_stacktrace


class RunHaskell(Language):

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        return ["runhaskell", file, *arguments]

    def solution(self, solution: Path, bundle: Bundle):
        haskell_solution(self, solution, bundle)

    def filter_dependencies(self,
                            bundle: Bundle,
                            files: List[str],
                            context_name: str) -> List[str]:
        return files

    def cleanup_description(self, namespace: str, description: str) -> str:
        return cleanup_description(self, namespace, description)

    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
        return haskell_cleanup_stacktrace(traceback, submission_file, reduce_all)
