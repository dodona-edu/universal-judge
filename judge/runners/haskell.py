"""
Support running exercises in Haskell.

Note that this is very preliminary support; a bunch of features are not supported yet or well-defined
yet, such as:

- Object functions
"""
from pathlib import Path
from typing import List, Union

from runners.config import LanguageConfig
from testplan import Context


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

    def needs_compilation(self) -> bool:
        return True

    def create_submission_code(self, context: Context, source: Union[Path, str], destination: Path):
        with open(source, 'r') as original:
            file = original.read()

        # noinspection PyTypeChecker
        with open(destination, 'w') as result:
            result.write(f"module {self.submission_name(context)} where\n\n")
            result.write(file)

    def execution_command(self) -> List[str]:
        # TODO: this currently interprets the files.
        #   Perhaps we could estimate if compilation is worthwhile based on the amount of code?
        name = self.context_name() + "." + self.file_extension()
        return ["runhaskell", name]

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        file = f"{self.evaluator_name()}.{self.file_extension()}"
        return ["runhaskell", file]

    def file_extension(self) -> str:
        return "hs"

    def compilation_command(self, files: List[str]) -> List[str]:
        # Don't want to compile contexts, since they need recompilation.
        f = [x for x in files if "Context" not in x]
        return ["ghc", "-S", *files, "-main-is", self.context_name()]

    def submission_name(self, context: Context) -> str:
        # In Haskell, the code is the same for all contexts.
        return context.object

    def context_name(self) -> str:
        return f"Context"

    def additional_files(self) -> List[str]:
        return ["Values.hs"]

    def rename_evaluator(self, code, name):
        return code.replace("evaluate", name, 2)
