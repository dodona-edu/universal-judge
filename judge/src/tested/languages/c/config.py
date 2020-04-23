from pathlib import Path
from typing import List, Mapping

from humps import decamelize

from tested.languages import Language
from tested.languages.config import CallbackResult, TypeSupport, executable_name
from tested.datatypes import (AdvancedNumericTypes as ant, AllTypes,
                              AdvancedSequenceTypes as ast, BasicObjectTypes as bot,
                              BasicSequenceTypes as bst)
from tested.features import Constructs
from tested.testplan import Plan
from tested.utils import fallback


class CConfig(Language):
    """
    Configuration for the C language.
    """

    def supports_evaluation(self):
        return False

    def initial_dependencies(self) -> List[str]:
        return ["values.h", "values.c"]

    def evaluator_dependencies(self) -> List[str]:
        raise NotImplementedError("C does not support evaluation")

    def generation_callback(self, files: List[str]) -> CallbackResult:
        main_file = files[-1]
        exec_file = Path(main_file).stem
        result = executable_name(exec_file)
        # Only include the values, to prevent multiple definitions.
        # c_files = [x for x in files if x.endswith(".c")]
        return ["gcc", "-std=c11", "-Wall", "values.c", main_file, "-o", result], [result]

    def execution_command(self,
                          cwd: Path,
                          file: str,
                          arguments: List[str]) -> List[str]:
        local_file = cwd / executable_name(Path(file).stem)
        return [str(local_file.absolute()), *arguments]

    def file_extension(self) -> str:
        return "c"

    def submission_name(self, plan: Plan) -> str:
        return "submission"

    def selector_name(self) -> str:
        return "selector"

    def context_name(self, tab_number: int, context_number: int) -> str:
        return f"context_{tab_number}_{context_number}"

    def conventionalise_namespace(self, class_name: str) -> str:
        return decamelize(class_name)

    def conventionalise_function(self, function_name: str) -> str:
        return decamelize(function_name)

    def needs_selector(self):
        return True

    def solution_callback(self, solution: Path, plan: Plan):
        # noinspection PyTypeChecker
        with open(solution, "r") as file:
            contents = file.read()
        # noinspection PyTypeChecker
        with open(solution, "w") as file:
            header = "#pragma once\n\n"
            result = header + contents.replace("main", "solution_main")
            file.write(result)

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
            ant.DOUBLE_EXTENDED:  TypeSupport.SUPPORTED,
            ant.FIXED_PRECISION:  TypeSupport.UNSUPPORTED,
            ast.ARRAY:            TypeSupport.SUPPORTED,
            ast.LIST:             TypeSupport.UNSUPPORTED,
            ast.TUPLE:            TypeSupport.UNSUPPORTED,
            bst.SET:              TypeSupport.UNSUPPORTED,
            bot.MAP:              TypeSupport.UNSUPPORTED,
        })

    def supported_constructs(self) -> Constructs:
        return (Constructs.MAIN | Constructs.NOTHING | Constructs.FUNCTION_CALL
                | Constructs.ASSIGNMENT)
