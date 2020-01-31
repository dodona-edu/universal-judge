"""
Module containing the base class with the expected methods for a language
configuration. To support a new language, it is often enough to subclass the
LanguageConfig class and implement the required templates.

For very exotic languages, it is possible to create a custom runner subclass,
but that will be a lot more work.
"""
from typing import List, Tuple

from features import Features
from testplan import Plan

CallbackResult = Tuple[List[str], List[str]]


class LanguageConfig:
    """
    Configuration for the runner.
    """

    def generation_callback(self, files: List[str]) -> CallbackResult:
        """
        Called to do the generation step. This function is responsible for
        returning the precompilation command, and a list of new dependencies. An
        implementation might abstract this logic and put it in a separate
        function, to re-use it in the compilation callback. By default, nothing
        is done here, and the dependencies are returned unchanged.
        :param files: The files that are destined for precompilation. These were
                      removed from the general dependencies. There are relative
                      filenames to the current directory.
        :return: A tuple, containing 1) the compilation command. If no
                 compilation is needed, an empty command may be used. Secondly,
                 the new dependencies, which are a list of names.
        """
        return [], files

    def execution_command(self, files: List[str], context_number: int) -> List[str]:
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

    def template_folders(self, programming_language: str) -> List[str]:
        """The name of the template folders to search."""
        return [programming_language]

    def template_extensions(self) -> List[str]:
        """Extensions a template can be in."""
        return [self.file_extension(), "mako"]

    def initial_dependencies(self) -> List[str]:
        """
        Return the initial dependencies. These are filenames, relative to the
        "templates" directory.
        """
        raise NotImplementedError

    def _get_main_file(self, files: List[str]) -> str:
        files = [x for x in files if x.startswith(self.context_name())]
        if len(files) != 1:
            raise AssertionError(f"The files must contain one main file, but got {len(files)} from {files}.")
        return files[0]

    def supported_features(self) -> Features:
        """
        The features supported by this language. The default implementation
        returns all features. If a language supports only a subset, it is
        recommended to explicitly enumerate the supported features instead
        (whitelist) instead of removing non-supported features (blacklist). This
        allows new features to be added without having to update the language.
        :return: The features supported by this language.
        """
        return (Features.OBJECTS | Features.EXCEPTIONS | Features.MAIN | Features.FUNCTION_CALL | Features.ASSIGNMENT
                | Features.LISTS | Features.SETS | Features.MAPS
                | Features.INTEGERS | Features.RATIONALS
                | Features.STRINGS
                | Features.BOOLEANS
                | Features.NULL)
