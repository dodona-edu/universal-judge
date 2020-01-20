import os

from typing import List

from runners.config import LanguageConfig, CallbackResult
from testplan import Plan


class JavaConfig(LanguageConfig):
    """Configuration for the Java language."""

    def initial_dependencies(self) -> List[str]:
        return ["Values.java", "json-simple-3.1.0.jar"]

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        cp = self._classpath_separator().join(self._get_classpath())
        return ["java", "-cp", f"{cp}:.", evaluator_name]

    def evaluator_name(self) -> str:
        return f"Evaluator"

    def conventionalise(self, function_name: str) -> str:
        return function_name

    def value_writer(self, name):
        return f"public void {name}(Object value) throws Exception {{send(value);}}"

    def exception_writer(self, name):
        return f"public void {name}(Exception e) throws Exception {{sendException(e);}}"

    def _get_classpath(self):
        return [x for x in self.initial_dependencies() if x.endswith(".jar")]

    def _classpath_separator(self):
        if os.name == 'nt':
            return ";"
        else:
            return ":"

    def execution_command(self, files: List[str]) -> List[str]:
        cp = self._classpath_separator().join(self._get_classpath() + ["."])
        c = ["java", "-cp", cp, self.context_name()]
        return c

    def file_extension(self) -> str:
        return "java"

    def generation_callback(self, files: List[str]) -> CallbackResult:
        return self.compilation_callback(files)

    def compilation_callback(self, files: List[str]) -> CallbackResult:
        others = [x for x in files if not x.endswith(".jar") and not x.endswith(".class")]
        jar_argument = self._classpath_separator().join(self._get_classpath() + ["."])
        c = ["javac", "-cp", jar_argument, *others]
        return c, [x.replace(".java", ".class") for x in files]

    def submission_name(self, plan: Plan) -> str:
        return plan.object

    def context_name(self) -> str:
        return f"Context"
