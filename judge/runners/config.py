"""
Module containing the base class with the expected methods for a language
configuration. To support a new language, it is often enough to subclass the
LanguageConfig class and implement the required templates.

For very exotic languages, it is possible to create a custom runner subclass,
but that will be a lot more work.
"""
import os
from typing import List, Tuple
from pathlib import Path

from features import Features
from testplan import Plan, FunctionCall
from dodona import AnnotateCode

CallbackResult = Tuple[List[str], List[str]]


def executable_name(basename: str) -> str:
    """Append .exe on Windows only."""
    if os.name == 'nt':
        return f"{basename}.exe"
    else:
        return basename


class LanguageConfig:
    """
    Configuration for the runner. Most of the language dependent options are
    collected in this class. More involved language dependent changes, such as
    code generation, are handled by the templates.
    """

    def generation_callback(self, files: List[str]) -> CallbackResult:
        """
        Called to do the generation step. This function is responsible for
        returning the precompilation command, and a list of new dependencies. By
        default, nothing is done here, and the dependencies are returned unchanged.
        :param files: The files that are destined for precompilation. These were
                      removed from the general dependencies. There are relative
                      filenames to the current directory.
        :return: A tuple, containing 1) the compilation command. If no
                 compilation is needed, an empty command may be used. Secondly,
                 the new dependencies, which are a list of names.
        """
        return [], files

    def evaluator_generation_callback(self, files: List[str]) -> CallbackResult:
        """
        Same as the generation_callback, but used for evaluators. By default,
        this function just calls generation_callback.
        """
        return self.generation_callback(files)

    def execution_command(self,
                          cwd: Path,
                          file: str,
                          dependencies: List[str],
                          arguments: List[str]) -> List[str]:
        """
        Get the command for executing a file with some arguments.
        :param cwd: The directory where the command will be execute. This must only
                    be used to specify the path the executable (first item in the
                    returned command). Files MUST NOT be prepended with the cwd.
        :param file: The "main" file to be executed.
        :param dependencies: A list of other available files.
        :param arguments: The arguments, e.g. other dependencies or execution
                          arguments.
        :return: A command that can be passed to the subprocess package.
        """
        raise NotImplementedError

    def file_extension(self) -> str:
        """The file extension for this language, without the dot."""
        raise NotImplementedError

    def submission_name(self, plan: Plan) -> str:
        """The name for the submission file."""
        raise NotImplementedError

    def selector_name(self) -> str:
        """The name of the selector module."""
        raise NotImplementedError

    def context_name(self, tab_number: int, context_number: int) -> str:
        raise NotImplementedError

    def conventionalise(self, function_name: str) -> str:
        """Apply a language's conventions to function name."""
        raise NotImplementedError

    def conventionalise_object(self, class_name: str) -> str:
        """Apply a language's conventions to a module name."""
        raise NotImplementedError

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

    def evaluator_dependencies(self) -> List[str]:
        """
        Additional dependencies for running a custom evaluator in this language.
        These dependencies are also relative to the "templates" directory, and
        are loaded after the regular dependencies.
        """
        return []

    def supported_features(self) -> Features:
        """
        The features supported by this language. The default implementation
        returns all features. If a language supports only a subset, it is
        recommended to explicitly enumerate the supported features instead
        (whitelist) instead of removing non-supported features (blacklist). This
        allows new features to be added without having to update the language.
        :return: The features supported by this language.
        """
        return (Features.OBJECTS | Features.EXCEPTIONS | Features.MAIN
                | Features.FUNCTION_CALL | Features.ASSIGNMENT
                | Features.LISTS | Features.SETS | Features.MAPS
                | Features.INTEGERS | Features.RATIONALS
                | Features.STRINGS
                | Features.BOOLEANS
                | Features.NULL)

    def supports(self, plan: Plan) -> bool:
        """
        Check if the given testplan is supported by this language.
        :param plan: The testplan to check.
        :return: True if supported.
        """
        required = plan.get_used_features()
        supported_features = self.supported_features()
        return supported_features & required != 0

    def solution_callback(self, solution: Path, plan: Plan):
        """
        An opportunity to modify the solution. By default, this does nothing.
        :param solution: Path to the solution and path for the modified solution.
        :param plan: The testplan.
        """
        pass

    def specific_evaluator_callback(self, function: FunctionCall) -> FunctionCall:
        """
        An opportunity to modify the function call used to call the language-
        specific evaluator. This allows injecting language dependent parameters.
        TODO: this is fairly ugly, is there a better way?
        By default, this does nothing.
        :param function: The function as produced by the judge.
        :return: The new function.
        """
        return function

    def context_dependencies_callback(self,
                                      context_name: str,
                                      dependencies: List[str]) -> List[str]:
        """
        An opportunity to modify the list of dependencies that is copied to the
        context-specific directory when executing. This can be used to, for example,
        not copy files for all contexts, but only those specific for the given
        context. By default, this does nothing.
        :param tab_number: The tab that is being executed.
        :param context_number: The context that is being executed.
        :param dependencies: The dependencies. Read-only: copy before modifying.
        :return: The new dependencies.
        """
        return dependencies

    def needs_selector(self):
        raise NotImplementedError

    def process_compiler_output(
            self,
            stdout: str,
            stderr: str
    ) -> List[AnnotateCode]:
        """Allows parsing error message to annotate the code."""
        return []
