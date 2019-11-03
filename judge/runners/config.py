"""Configuration for languages, making implementing runners fairly easy."""
from typing import List

from testplan import Plan


class LanguageConfig:
    """
    Configuration for the runner
    """

    # TODO: proper support for language features.
    def supports_top_level_functions(self) -> bool:
        """If the language supports top level functions."""
        raise NotImplementedError

    def needs_compilation(self) -> bool:
        """If the language needs compilation."""
        raise NotImplementedError

    def compilation_command(self, files: List[str]) -> List[str]:
        """Compile some files."""
        if self.needs_compilation():
            raise NotImplementedError
        else:
            return []

    def execution_command(self, context_id: str) -> List[str]:
        raise NotImplementedError

    def file_extension(self) -> str:
        """The file extension for this language."""
        raise NotImplementedError

    def submission_name(self, plan: Plan) -> str:
        """Produce a name for the submission file, without extension."""
        raise NotImplementedError

    def context_name(self, context_id: str) -> str:
        """Produce a name for the context file, without extension."""
        raise NotImplementedError

    def additional_files(self) -> List[str]:
        raise NotImplementedError

    def needs_main(self):
        """If the language needs a main function."""
        raise NotImplementedError
