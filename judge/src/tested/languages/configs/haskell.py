from os import PathLike
from pathlib import Path
from typing import List, Union

from humps import pascalize

from .. import Language
from ..config import CallbackResult, executable_name
from ...features import Constructs
from ...serialisation import StringType, StringTypes, FunctionCall
from ...testplan import Plan


class HaskellConfig(Language):
    """
    Configuration for the Haskell language, in compilation mode. This means that all
    code is first compiled with GHC, before it is executed.
    """

    def needs_selector(self):
        return True

    def selector_name(self) -> str:
        return "Selector"

    def initial_dependencies(self) -> List[str]:
        return ["Values.hs", "SpecificEvaluationUtils.hs"]

    def evaluator_dependencies(self) -> List[str]:
        return ["EvaluationUtils.hs"]

    def generation_callback(self, files: List[str]) -> CallbackResult:
        # TODO: indicate the context_testcase file somehow?
        main_file = files[-1]
        exec_file = main_file.rstrip(".hs")
        return (["ghc", main_file, "-context_testcase-is", exec_file],
                [executable_name(exec_file)])

    def conventionalise_function(self, function_name: str) -> str:
        return function_name

    def conventionalise_object(self, class_name: str) -> str:
        return pascalize(class_name)

    def execution_command(self, cwd: Path, file: str, dependencies: List[str],
                          arguments: List[str]) -> List[str]:
        local_file = cwd / file
        return [str(local_file.absolute()), *arguments]

    def file_extension(self) -> str:
        return "hs"

    def submission_name(self, plan: Plan) -> str:
        return plan.namespace

    def context_name(self, tab_number: int, context_number: int) -> str:
        return f"Context_{tab_number}_{context_number}"

    def supported_features(self) -> Constructs:
        return (Constructs.MAIN | Constructs.FUNCTION_CALL | Constructs.ASSIGNMENT
                | Constructs.LISTS | Constructs.SETS | Constructs.MAPS
                | Constructs.INTEGERS | Constructs.RATIONALS | Constructs.STRINGS
                | Constructs.BOOLEANS)

    def solution_callback(self, solution: Union[Path, PathLike], plan: Plan):
        """Support implicit modules if needed."""
        if plan.config_for("haskell").get("implicitModule", True):
            name = self.submission_name(plan)
            with open(solution, "r") as file:
                contents = file.read()
            with open(solution, "w") as file:
                result = f"module {name} where\n" + contents
                file.write(result)

    def specific_evaluator_callback(self, function: FunctionCall) -> FunctionCall:
        """Inject file path to expression to specific evaluator"""
        # Prepend value_file to front of arguments.
        value_file = StringType(
            type=StringTypes.IDENTIFIER,
            data="value_file"
        )
        arguments = [value_file] + function.arguments
        return FunctionCall(
            type=function.type,
            name=function.name,
            namespace=function.namespace,
            arguments=arguments
        )
