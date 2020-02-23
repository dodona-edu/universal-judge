import os
from pathlib import Path

from humps import pascalize, camelize

from typing import List
from runners.config import LanguageConfig, CallbackResult
from testplan import Plan


class JavaConfig(LanguageConfig):
    """
    Configuration for the Java language.
    This configuration is for the traditional way of executing Java, meaning it
    is first compiled and then executed.
    """

    def initial_dependencies(self) -> List[str]:
        return ["Values.java", "json-simple-3.1.0.jar", "AbstractEvaluator.java",
                "AbstractSpecificEvaluator.java"]

    def evaluator_dependencies(self) -> List[str]:
        return ["AbstractCustomEvaluator.java"]

    def generation_callback(self, files: List[str]) -> CallbackResult:
        others = [self.conventionalise_object(x) for x in files if
                  not x.endswith(".jar")]
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
        return plan.object

    def conventionalise(self, function_name: str) -> str:
        return camelize(function_name)

    def conventionalise_object(self, class_name: str) -> str:
        return pascalize(class_name)

    def selector_name(self) -> str:
        return "Selector"

    def context_name(self, tab_number: int, context_number: int) -> str:
        return f"Context_{tab_number}_{context_number}"

    def _get_classpath(self):
        return [x for x in self.initial_dependencies() if x.endswith(".jar")]

    def _classpath_separator(self):
        if os.name == 'nt':
            return ";"
        else:
            return ":"
