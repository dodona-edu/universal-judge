from pathlib import Path
from typing import List

from runners.common import _get_identifier, ConfigurableRunner, LanguageConfig
from tested import Config
from testplan import Plan


class JavaConfig(LanguageConfig):

    def supports_top_level_functions(self) -> bool:
        return False

    def needs_compilation(self) -> bool:
        return True

    def execution_command(self, context_id: str, path: Path) -> List[str]:
        return ["java", "-cp", str(path), self.context_name(context_id)]

    def file_extension(self) -> str:
        return "java"

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


class JavaRunner(ConfigurableRunner):

    def __init__(self, config: Config):
        super().__init__(config, JavaConfig())
        self.identifier = _get_identifier()
