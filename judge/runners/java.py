from pathlib import Path
from typing import List

from runners.config import LanguageConfig
from testplan import Plan


class JavaConfig(LanguageConfig):

    def needs_main(self):
        return True

    def supports_top_level_functions(self) -> bool:
        return False

    def needs_compilation(self) -> bool:
        return True

    def execution_command(self, context_id: str) -> List[str]:
        return ["java", "-cp", ".", self.context_name(context_id)]

    def file_extension(self) -> str:
        return "java"

    def compilation_command(self, files: List[str]) -> List[str]:
        return ["javac", *files]

    def submission_name(self, plan: Plan) -> str:
        # In Java, there can be only one class per file. This means the class name
        # specified in all main functions should be the same. Right now we take the name of
        # the first context.
        try:
            name = plan.tabs[0].contexts[0].execution.input.function.object
        except IndexError:
            # There are no tabs or no contexts. In that case we use "Main" as default, but it
            # isn't relevant, as there will be no tests.
            name = "Main"
        return name

    def context_name(self, context_id: str) -> str:
        return f"Context{context_id}"

    def additional_files(self) -> List[str]:
        return ["Values.java"]
