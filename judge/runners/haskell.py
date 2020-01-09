"""
Support running exercises in Haskell.

Note that this is very preliminary support; a bunch of features are not supported yet or well-defined
yet, such as:

- Object functions
"""
from typing import List

from runners.config import LanguageConfig
from testplan import Plan


class HaskellConfig(LanguageConfig):
    """Configuration for the Haskell language, with limitation (see module docs)."""

    def exception_writer(self, name):
        return ""  # Not applicable to Haskell

    def evaluator_name(self) -> str:
        return f"Evaluator"

    def conventionalise(self, function_name: str) -> str:
        return function_name

    def value_writer(self, name):
        return f"{name} :: Typeable a => a -> IO ()\n{name} = send"

    def execution_command(self, files: List[str]) -> List[str]:
        # TODO: this currently interprets the files.
        #   Perhaps we could estimate if compilation is worthwhile based on the amount of code?
        name = self.context_name() + "." + self.file_extension()
        return ["runhaskell", name]

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        file = f"{self.evaluator_name()}.{self.file_extension()}"
        return ["runhaskell", file]

    def file_extension(self) -> str:
        return "hs"

    def submission_name(self, plan: Plan) -> str:
        return plan.object

    def context_name(self) -> str:
        return f"Context"

    def initial_dependencies(self) -> List[str]:
        return ["Values.hs"]

    def rename_evaluator(self, code, name):
        return code.replace("evaluate", name, 2)
