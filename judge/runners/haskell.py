"""
Support running exercises in Haskell.

Note that this is very preliminary support; a bunch of features are not supported yet or well-defined
yet, such as:

- Setting program arguments
- Object functions
- etc.
"""
from typing import List

from runners.config import LanguageConfig
from testplan import Context


class HaskellConfig(LanguageConfig):
    """Configuration for the Haskell language, with limitation (see module docs)."""

    def evaluator_name(self, context_id: str) -> str:
        return f"Evaluator{context_id}"

    def conventionalise(self, function_name: str) -> str:
        return function_name

    def value_writer(self, name):
        return f"{name} :: Typeable a => a -> IO ()\n{name} = send"

    def needs_compilation(self) -> bool:
        return True

    def execution_command(self, context_id: str) -> List[str]:
        # TODO: this currently interprets the files.
        #   Perhaps we could estimate if compilation is worthwhile based on the amount of code?
        name = self.context_name(context_id) + "." + self.file_extension()
        return ["runhaskell", name, "-main-is", self.context_name(context_id)]

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        file = f"{self.evaluator_name('eval')}.{self.file_extension()}"
        return ["runhaskell", file]

    def file_extension(self) -> str:
        return "hs"

    def compilation_command(self, files: List[str]) -> List[str]:
        # Don't want to compile contexts, since they need recompilation.
        f = [x for x in files if "Context" not in x]
        return ["ghc", "-c", *f, "-no-hs-main"]

    def submission_name(self, context_id: str, context: Context) -> str:
        # In Haskell, the code is the same for all contexts.
        return context.object

    def context_name(self, context_id: str) -> str:
        return f"Context{context_id}"

    def additional_files(self) -> List[str]:
        return ["Values.hs"]

    def rename_evaluator(self, code, name):
        return code.replace("evaluate", name, 2)
