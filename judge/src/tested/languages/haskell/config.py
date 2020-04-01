from os import PathLike
from pathlib import Path
from typing import List, Union, Mapping

from humps import pascalize

from tested.languages import Language
from tested.languages.config import CallbackResult, executable_name, TypeSupport
from tested.features import Constructs
from tested.serialisation import StringType, StringTypes, FunctionCall
from tested.testplan import Plan
from tested.datatypes import (AdvancedNumericTypes as ant, AllTypes,
                              AdvancedSequenceTypes as ast,
                              BasicSequenceTypes as bst, BasicObjectTypes as bot)
from tested.utils import fallback


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
        main_file = files[-1]
        exec_file = main_file.rstrip(".hs")
        return (["ghc", main_file, "-main-is", exec_file],
                [executable_name(exec_file)])

    def conventionalise_function(self, function_name: str) -> str:
        return function_name

    def conventionalise_namespace(self, class_name: str) -> str:
        return pascalize(class_name)

    def execution_command(self, cwd: Path, file: str, dependencies: List[str],
                          arguments: List[str]) -> List[str]:
        local_file = cwd / file
        return [str(local_file.absolute()), *arguments]

    def file_extension(self) -> str:
        return "hs"

    def context_name(self, tab_number: int, context_number: int) -> str:
        return f"Context_{tab_number}_{context_number}"

    def supported_constructs(self) -> Constructs:
        return Constructs.MAIN | Constructs.FUNCTION_CALL | Constructs.ASSIGNMENT

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

    # noinspection DuplicatedCode
    def type_support_map(self) -> Mapping[AllTypes, TypeSupport]:
        return fallback(super().type_support_map(), {
            ant.INT_8:            TypeSupport.SUPPORTED,
            ant.U_INT_8:          TypeSupport.SUPPORTED,
            ant.INT_16:           TypeSupport.SUPPORTED,
            ant.U_INT_16:         TypeSupport.SUPPORTED,
            ant.INT_32:           TypeSupport.SUPPORTED,
            ant.U_INT_32:         TypeSupport.SUPPORTED,
            ant.INT_64:           TypeSupport.SUPPORTED,
            ant.U_INT_64:         TypeSupport.SUPPORTED,
            ant.SINGLE_PRECISION: TypeSupport.SUPPORTED,
            ant.DOUBLE_EXTENDED:  TypeSupport.UNSUPPORTED,  # TODO: maybe?
            ant.FIXED_PRECISION:  TypeSupport.UNSUPPORTED,  # TODO: maybe?
            ast.ARRAY:            TypeSupport.UNSUPPORTED,  # TODO: maybe?
            bst.SET:              TypeSupport.UNSUPPORTED,  # TODO: maybe?
            bot.MAP:              TypeSupport.UNSUPPORTED,  # TODO: maybe?
            ast.LIST:             TypeSupport.SUPPORTED,
            ast.TUPLE:            TypeSupport.SUPPORTED
        })
