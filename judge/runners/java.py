import os
from typing import List

from runners.config import LanguageConfig
from testplan import Context


class JavaConfig(LanguageConfig):
    """Configuration for the Java language."""

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        cp = ":".join(self._get_classpath())
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
        return [x for x in self.additional_files() if x.endswith(".jar")]

    def _classpath_separator(self):
        if os.name == 'nt':
            return ";"
        else:
            return ":"

    def execution_command(self, files: List[str]) -> List[str]:
        cp = self._classpath_separator().join(self._get_classpath() + ["."])
        return ["java", "-cp", cp, self.context_name()]

    def file_extension(self) -> str:
        return "java"

    def compilation_command(self, files: List[str]) -> List[str]:
        others = [x for x in files if not x.endswith(".jar")]
        jar_argument = self._classpath_separator().join(self._get_classpath())
        return ["javac", "-cp", jar_argument, *others]

    def submission_name(self, context: Context) -> str:
        # In Java, the code is the same for all contexts.
        return context.object

    def context_name(self) -> str:
        return f"Context"

    def additional_files(self) -> List[str]:
        return ["Values.java", "json-simple-3.1.0.jar"]
