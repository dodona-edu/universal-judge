from typing import List, Optional

from runners.config import LanguageConfig
from testplan import Context


class JavaScriptedConfig(LanguageConfig):
    """
    Configuration for the Java language in script mode.
    This does not support most features.
    TODO: system for indicating required/supported features.
    """

    def value_writer(self, name):
        return ""

    def needs_main(self):
        return False

    def needs_compilation(self) -> bool:
        return True

    def execution_command(self) -> List[str]:
        return ["java", "-cp", ".", self.context_name()]

    def file_extension(self) -> str:
        return "java"

    def compilation_command(self, files: List[str]) -> List[str]:
        return ["javac", *files]

    def submission_name(self, context: Context) -> Optional[str]:
        return "Script"

    def context_name(self) -> str:
        return "Context"

    def additional_files(self) -> List[str]:
        return []
