"""Configuration for languages, making implementing runners fairly easy."""
import shutil
from pathlib import Path
from typing import List, Union

from tested import Config
from testplan import Context


class LanguageConfig:
    """
    Configuration for the runner
    """

    def compilation_command(self, files: List[str]) -> List[str]:
        """Compile some files."""
        return []

    def execution_command(self, files: List[str]) -> List[str]:
        """Get the command for executing the code."""
        raise NotImplementedError

    def create_submission_code(self, context: Context, source: Union[Path, str], destination: Path):
        """Create the submission code"""
        # noinspection PyTypeChecker
        shutil.copy2(source, destination)

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        """Get the command for evaluating an evaluator."""
        raise NotImplementedError

    def file_extension(self) -> str:
        """The file extension for this language, without dot."""
        raise NotImplementedError

    def submission_name(self, context: Context) -> str:
        """The name for the submission file."""
        raise NotImplementedError

    def user_friendly_submission_name(self, context: Context):
        if context.object:
            return context.object
        else:
            return ""

    def context_name(self) -> str:
        """The name of the context file."""
        raise NotImplementedError

    def evaluator_name(self) -> str:
        """The name for the evaluator file."""
        raise NotImplementedError

    def additional_files(self) -> List[str]:
        """Additional files that will be available to the context tests."""
        raise NotImplementedError

    def value_writer(self, name):
        """Return the code needed to write values to the file."""
        raise NotImplementedError

    def exception_writer(self, name):
        """Return the code needed to write exceptions to the file."""
        raise NotImplementedError

    def conventionalise(self, function_name: str) -> str:
        """Apply a language's conventions to function name."""
        raise NotImplementedError

    def rename_evaluator(self, code, name):
        """Replace the evaluate function name"""
        return code.replace("evaluate", name, 1)

    def template_folders(self, config: Config) -> List[str]:
        """The name of the template folders to search."""
        return [config.programming_language]

    def template_extensions(self) -> List[str]:
        """Extensions a template can be in."""
        return [self.file_extension(), "mako"]

    def get_all_testcases(self, context: Context):
        """Get all testcases. Scripts may use this to inject a main testcase if needed."""
        return context.all_testcases()