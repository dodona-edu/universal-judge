from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages import Language
from tested.languages.config import CallbackResult, executable_name, Command, Config


class Haskell(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        main_ = files[-1]
        exec_ = main_.rstrip(".hs")
        return ["ghc", "-O0", main_, "-main-is", exec_], [executable_name(exec_)]

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        local_file = cwd / file
        return [str(local_file.absolute()), *arguments]

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
