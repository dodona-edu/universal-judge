import os
from pathlib import Path
from typing import List, Mapping

from humps import pascalize, camelize

from .. import Language
from ..config import CallbackResult, TypeSupport
from ...datatypes import AdvancedNumericTypes as ant, AllTypes
from ...datatypes import AdvancedSequenceTypes as ast
from ...testplan import Plan
from ...utils import fallback

CONTEXT_PREFIX = "Context_"


class JavaConfig(Language):
    """
    Configuration for the Java language.
    This configuration is for the traditional way of executing Java, meaning it
    is first compiled and then executed.
    """

    def initial_dependencies(self) -> List[str]:
        return ["Values.java", "AbstractEvaluator.java",
                "AbstractSpecificEvaluator.java"]

    def evaluator_dependencies(self) -> List[str]:
        return ["AbstractCustomEvaluator.java"]

    def generation_callback(self, files: List[str]) -> CallbackResult:
        others = [x for x in files if not x.endswith(".jar")]
        jar_argument = self._classpath_separator().join(
            self._get_classpath() + ["."])
        c = ["javac", "-cp", jar_argument, *others]
        return c, [x.replace(".java", ".class") for x in files]

    def execution_command(self, cwd: Path, file: str, dependencies: List[str],
                          arguments: List[str]) -> List[str]:
        cp = self._classpath_separator().join(self._get_classpath() + ["."])
        name = Path(file).stem
        return ["java", "-cp", cp, self.conventionalise_object(name), *arguments]

    def file_extension(self) -> str:
        return "java"

    def submission_name(self, plan: Plan) -> str:
        return plan.namespace

    def conventionalise_function(self, function_name: str) -> str:
        return camelize(function_name)

    def conventionalise_object(self, class_name: str) -> str:
        return pascalize(class_name)

    def selector_name(self) -> str:
        return "Selector"

    def context_name(self, tab_number: int, context_number: int) -> str:
        return f"{CONTEXT_PREFIX}{tab_number}_{context_number}"

    def _get_classpath(self):
        return [x for x in self.initial_dependencies() if x.endswith(".jar")]

    def _classpath_separator(self):
        if os.name == 'nt':
            return ";"
        else:
            return ":"

    def needs_selector(self):
        return True

    def context_dependencies_callback(self,
                                      context_name: str,
                                      dependencies: List[str]) -> List[str]:
        return [x for x in dependencies
                if not x.startswith(CONTEXT_PREFIX)
                   or x.startswith(f"{context_name}.")]

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
            ant.FIXED_PRECISION:  TypeSupport.SUPPORTED,
            ast.ARRAY:            TypeSupport.SUPPORTED,
            ast.LIST:             TypeSupport.SUPPORTED,
            ast.TUPLE:            TypeSupport.UNSUPPORTED
        })
