import os

from typing import List

from runners.config import LanguageConfig, CallbackResult
from testplan import Plan


class JavaConfig(LanguageConfig):
    """
    Configuration for the Java language.

    This configuration is for the traditional way of executing Java, meaning it
    is first compiled and then executed.
    JShell is supported by another configuration class, but re-uses parts of the
    Java configuration. Implementers of this class should be aware of that.
    """

    def initial_dependencies(self) -> List[str]:
        return ["Values.java", "json-simple-3.1.0.jar"]

    def evaluator_dependencies(self) -> List[str]:
        return ["Evaluator.java"]

    def value_writer(self, name):
        return f"public void {name}(Object value) throws Exception {{send(value);}}"

    def exception_writer(self, name):
        return f"public void {name}(Exception e) throws Exception {{sendException(e);}}"

    def generation_callback(self, files: List[str]) -> CallbackResult:
        others = [x for x in files if not x.endswith(".jar")]
        jar_argument = self._classpath_separator().join(self._get_classpath() + ["."])
        c = ["javac", "-cp", jar_argument, *others]
        return c, [x.replace(".java", ".class") for x in files]

    def execution_command(self, file: str, dependencies: List[str], arguments: List[str]) -> List[str]:
        cp = self._classpath_separator().join(self._get_classpath() + ["."])
        return ["java", "-cp", cp, self.context_name(), *arguments]

    def file_extension(self) -> str:
        return "java"

    def submission_name(self, plan: Plan) -> str:
        return plan.object

    def context_name(self) -> str:
        return f"Context"

    def evaluator_name(self) -> str:
        return f"Evaluator"

    def conventionalise(self, function_name: str) -> str:
        return function_name

    def _get_classpath(self):
        return [x for x in self.initial_dependencies() if x.endswith(".jar")]

    def _classpath_separator(self):
        if os.name == 'nt':
            return ";"
        else:
            return ":"
