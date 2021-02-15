from pathlib import Path
from typing import List, Tuple

from tested.configs import Bundle
from tested.dodona import Message, AnnotateCode
from tested.languages.config import CallbackResult, executable_name, Command, \
    Config, Language
from tested.languages.utils import haskell_solution, cleanup_description, \
    haskell_cleanup_stacktrace


# TODO: advanced type don't work very good at the moment.
class Haskell(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        main_ = files[-1]
        exec_ = main_.rstrip(".hs")
        return ["ghc", "-fno-cse", "-fno-full-laziness", "-O0", main_, "-main-is",
                exec_], [executable_name(exec_)]

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        local_file = cwd / file
        return [str(local_file.absolute()), *arguments]

    def solution(self, solution: Path, bundle: Bundle):
        haskell_solution(self, solution, bundle)

    def cleanup_description(self, namespace: str, description: str) -> str:
        return cleanup_description(self, namespace, description)

    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
        return haskell_cleanup_stacktrace(traceback, submission_file, reduce_all)

    def compiler_output(
            self, namespace: str, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        return [], [], "", haskell_cleanup_stacktrace(
            stderr,
            self.with_extension(self.conventionalize_namespace(namespace))
        )
