"""
Module containing the base class with the expected methods for a language configuration. To support
a new language, it is often enough to subclass the LanguageConfig class and implement the required
templates.

For very exotic languages, it is possible to create a custom runner subclass, but that will be a lot
more work.
"""
from typing import List, Tuple

from tested import Config
from testplan import Plan

CallbackResult = Tuple[List[str], List[str]]


class LanguageConfig:
    """
    Configuration for the runner.
    """

    def pre_compilation_callback(self, files: List[str]) -> CallbackResult:
        """
        Called to do the precompilation step. This function is responsible for returning the
        precompilation command, and a list of new dependencies. An implementation might abstract
        this logic and put it in a separate function, to re-use it in the compilation callback.
        By default, nothing is done here, and the dependencies are returned unchanged.
        :param files: The files that are destined for precompilation. These were removed from the
                      general dependencies. There are relative filenames to the current directory.
        :return: A tuple, containing 1) the compilation command. If no compilation is needed, an
                 empty command may be used. Secondly, the new dependencies, which are a list of
                 names.
        """
        return [], files

    def compilation_callback(self, files: List[str]) -> CallbackResult:
        """
        Called to do the compilation step. This function is responsible for returning the
        compilation command, and a list of new dependencies. An implementation might abstract this
        logic and put it in a separate function, to re-use it in the compilation callback.
        By default, nothing is done here, and the dependencies are returned unchanged.
        :param files: The files that are destined for compilation. These were removed from the
                      general dependencies. There are relative filenames to the current directory.
        :return: A tuple, containing 1) the compilation command. If no compilation is needed, an
                 empty command may be used. Secondly, the new dependencies, which are a list of
                 names.
        """
        return [], files

    def execution_command(self, files: List[str]) -> List[str]:
        """Get the command for executing the code."""
        raise NotImplementedError

    def execute_evaluator(self, evaluator_name: str) -> List[str]:
        """Get the command for executing an evaluator."""
        raise NotImplementedError

    def file_extension(self) -> str:
        """The file extension for this language, without the dot."""
        raise NotImplementedError

    def submission_name(self, plan: Plan) -> str:
        """The name for the submission file."""
        raise NotImplementedError

    def context_name(self) -> str:
        """The name of the context file."""
        raise NotImplementedError

    def evaluator_name(self) -> str:
        """The name for the evaluator file."""
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

    def initial_dependencies(self) -> List[str]:
        """
        Return the initial dependencies. These are filenames, relative to the templates directory.
        """
        raise NotImplementedError
