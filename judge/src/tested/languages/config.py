"""
Module containing the base class with the channel methods for a language
configuration. To support a new language, it is often enough to subclass the
Language class and implement the required templates.

For very exotic configs, it is possible to create a custom runner subclass,
but that will be a lot more work.
"""
import os
from collections import defaultdict
from enum import Enum, auto
from pathlib import Path
from typing import List, Tuple, Mapping

from ..configs import Bundle
from ..datatypes import AllTypes
from ..dodona import AnnotateCode, Message
from ..features import Constructs
from ..serialisation import FunctionCall
from ..testplan import Plan

CallbackResult = Tuple[List[str], List[str]]


def executable_name(basename: str) -> str:
    """Append .exe on Windows only."""
    if os.name == 'nt':
        return f"{basename}.exe"
    else:
        return basename


class TypeSupport(Enum):
    SUPPORTED = auto()
    UNSUPPORTED = auto()
    REDUCED = auto()


class Language:
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

    def post_generation_callback(self, directory: Path, files: List[str]) \
            -> List[str]:
        """
        Called after the generation step has run. Normally, the generation callback
        returns the resulting files. However, in some languages, such as Java, the
        generated files depend on the contents of the compiled files.
        :param directory: The directory where the files were compiled.
        :param files: The files as returned by the generation callback.
        :return: The new files.
        """
        return files

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
        :param file: The "context_testcase" file to be executed.
        :param dependencies: A list of other available files.
        :param arguments: The arguments, e.g. other dependencies or execution
                          arguments.
        :return: A command that can be passed to the subprocess package.
        """
        raise NotImplementedError

    def file_extension(self) -> str:
        """The file extension for this language, without the dot."""
        raise NotImplementedError

    def with_extension(self, file_name: str) -> str:
        return f"{file_name}.{self.file_extension()}"

    def submission_name(self, plan: Plan) -> str:
        """The name for the submission file."""
        return plan.namespace

    def selector_name(self) -> str:
        """The name of the selector module."""
        raise NotImplementedError

    def context_name(self, tab_number: int, context_number: int) -> str:
        raise NotImplementedError

    def conventionalise_function(self, function_name: str) -> str:
        """Apply a language's conventions to function name."""
        raise NotImplementedError

    def conventionalise_namespace(self, class_name: str) -> str:
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

    def supported_constructs(self) -> Constructs:
        """
        A flag representing which features this language supports. By default, all
        features are returned.

        Languages can declare missing support for features in one of two ways:

        - Enumerating all supported features. This is safe against new features.
        - Explicitly disallowing some features. This also means the language will
          automatically support new features when they are added to TESTed.

        :return: The features supported by this language.
        """
        return Constructs.ALL

    def type_support_map(self) -> Mapping[AllTypes, TypeSupport]:
        """
        Return a map containing the support for advanced types. The returned dict
        influences how types are used:

        - If a type is not present in the keys of the dict, it will be mapped to
          its basic type.
        - If a type is mapped to another `AdvancedType` (often itself), it means
          this language also supports the advanced type in question. There will be
          no fallback to the basic types.
        - If a type is mapped to None, the language does not support the advanced
          type. Testplans which contain this advanced type will not be executable in
          this language.

        Note: support for basic types is not done with this method, but uses the
        features functionality. If a language has no support for a basic type, all
        advanced types that map to this basic type will also not be supported.

        :return: The typing support dict. By default, all types are mapped to their
                 basic type.
        """
        return defaultdict(lambda: TypeSupport.REDUCED)

    def solution_callback(self, solution: Path, bundle: Bundle):
        """
        An opportunity to modify the solution. By default, this does nothing.
        :param solution: Path to the solution and path for the modified solution.
        :param bundle: The configuration bundle.
        """
        pass

    def specific_evaluator_callback(self, function: FunctionCall) -> FunctionCall:
        """
        An opportunity to modify the function expression used to expression the
        language-specific evaluator. This allows injecting language dependent
        parameters.
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
        :param context_name: The tab that is being executed.
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
    ) -> Tuple[List[Message], List[AnnotateCode]]:
        """
        Callback for processing compiler output.

        :param stdout:
        :param stderr:

        :return: A list of messages and annotations. None indicates that the
                 callback did not handle the output; TESTed will show it instead.
        """
        return [], []

    def run_linter(self, bundle: Bundle, submission: Path, remaining: float) \
            -> Tuple[List[Message], List[AnnotateCode]]:
        """
        Run a linter or other code analysis tools on the submission.
        The messages that are output will be passed to Dodona.
        By default, this does nothing.

        Note that you should not modify the solution file. There is no guarantee
        that this solution is the one that will be evaluated.

        :param bundle: The configuration bundle.
        :param submission: The path to the submission.
        :param remaining: The time the judge can use.

        :return: A list of messages and annotations.
        """
        return [], []

    def supports_evaluation(self):
        return True
