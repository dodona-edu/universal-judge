from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages import Language
from tested.languages.config import CallbackResult, executable_name, Command


class HaskellConfig(Language):

    def c_compilation(self, files: List[str]) -> CallbackResult:
        main_ = files[-1]
        exec_ = main_.rstrip(".hs")
        return ["ghc", "-O0", main_, "-main-is", exec_], [executable_name(exec_)]

    def c_execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        local_file = cwd / file
        return [str(local_file.absolute()), *arguments]

    def c_solution(self, solution: Path, bundle: Bundle):
        """Support implicit modules if needed."""
        if bundle.config.config_for().get("implicitModule", True):
            name = self.c_submission_name(bundle.plan)
            # noinspection PyTypeChecker
            with open(solution, "r") as file:
                contents = file.read()
            # noinspection PyTypeChecker
            with open(solution, "w") as file:
                result = f"module {name} where\n" + contents
                file.write(result)
