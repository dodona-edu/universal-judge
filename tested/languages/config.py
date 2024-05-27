"""
The configuration class for a programming language.

This class is the API between the core of TESTed and the language-specific details.
Everything that depends on the programming language passes through this class.
"""

import logging
import typing
from abc import ABC, abstractmethod
from collections.abc import Callable
from pathlib import Path
from typing import NotRequired, Optional, TypedDict

from tested.datatypes import AllTypes, ExpressionTypes
from tested.dodona import AnnotateCode, Message, Status
from tested.features import Construct, TypeSupport
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    Conventionable,
    NamingConventions,
    conventionalize_namespace,
    submission_name,
)
from tested.serialisation import Statement, Value

if typing.TYPE_CHECKING:
    from tested.configs import GlobalConfig
    from tested.languages.generation import PreparedExecutionUnit

Command = list[str]
FileFilter = Callable[[Path], bool]
CallbackResult = tuple[Command, list[str] | FileFilter]

_logger = logging.getLogger(__name__)


class TypeDeclarationMetadata(TypedDict):
    names: dict[AllTypes, str | tuple[bool, str]]
    inner_names: NotRequired[dict[AllTypes, str]]
    nested: NotRequired[tuple[str, str]]
    nested_overrides: NotRequired[dict[AllTypes, tuple[str, str]]]
    prompt: NotRequired[str]
    natural_overrides: NotRequired[dict[str, dict[AllTypes, tuple[str, str]]]]
    exception: NotRequired[str]


class Language(ABC):
    """
    Abstract base class for a programming language.

    You should implement all abstract methods in a subclass to properly implement
    support for a programming language. Some methods have a default value, meaning
    you can optionally override them, but it is not required.

    When overriding methods, it is always a good idea to consult the docs in the
    base class for information.

    Note that an instance of this class is linked to a specific test suite: if you
    want to run for another test suite, you must create a new instance.
    """

    config: Optional["GlobalConfig"]

    def __init__(self, config: Optional["GlobalConfig"]):
        """
        :param config: If the config is None, only "config" methods will work.
        """
        self.config = config

    def compilation(self, files: list[str]) -> CallbackResult:
        """
        Callback for generating the compilation command.

        Files
        -----

        The files parameter contains the dependencies which TESTed assumes can be
        useful for compilation. This generally includes the `dependencies` from the
        json config, the submission and either the context or the context and the
        selector. By convention, the last file in the list is the file containing
        the "main" function. The files are in the form of the filename, extension
        including.

        All files are guaranteed to be present in the same directory as the main
        file. You are not obligated to actually use all these files. In some
        languages, the compiler might find these dependencies on its own. Other
        compilers need all files.

        For example, the files parameter might look like this::

            ["values.py", "evaluation_utils.py", "context_0_0.py"]

        Compilation command
        -------------------

        The compilation command that is returned is a list of command parts, which
        will be passed to the `subprocess` module. The result of the compilation
        command must be an executable file (or files), that can be executed on the
        command line.

        Resulting files
        ---------------

        Additionally, TESTed needs to know which files are still relevant after
        compilation (to copy them to the execution folder). For example, in Python
        only *.pyc files are needed after compilation, not the *.py files.

        To this end, you can either return a static list of resulting files or a
        filter function:

        - The static list is useful if you know which files are generated. For
          example, in Python, each .py file will result in one .pyc file. In C,
          the result will be one executable.
        - The callback will be executed on all files in the directory after the
          compilation command has been executed. See below for more information.

        Note that the convention that the executable file is the last file in the
        list must be respected in the returned list as well.

        The callback function receives a filename as argument.

        Non-compiling languages
        -----------------------
        Languages that do not need compilation must return an empty list as
        compilation command, and can return the ``files`` unchanged. This is also
        the default implementation of this function.

        Parameters
        ----------

        :param files: A suggestion containing the dependencies TESTed thinks might
                      be useful to compile. By convention, the last file in the list
                      is the file containing the "main" function.

        :return: The compilation command and either the resulting files or a filter
                 for the resulting files.
        """
        return [], files

    @abstractmethod
    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        """
        Callback for generating the execution command.

        The execution command must execute the file given by the ``file`` parameter.
        When executing the file, the ``arguments`` MUST be passed to the programme.
        The returned command will be passed to the subprocess module.

        The ``cwd`` parameter is mainly useful for executables. Since those are not
        on the PATH, you should use an absolute path to those instead of a relative
        one.

        :param cwd: The directory in which the ``file`` is.
        :param file: The file to execute.
        :param arguments: Arguments that must be passed to the execution.

        :return: The execution command.
        """
        raise NotImplementedError

    def get_string_quote(self) -> str:
        """
        :return: The symbol used to quote strings.
        """
        return '"'

    @abstractmethod
    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        """
        Return naming conventions for this language.

        This should return a dictionary containing a mapping of "conventionable"
        items mapped on their naming convention.

        The default for missing conventionable items is snake case.
        :return: A mapping for the naming conventions.
        """
        raise NotImplementedError

    @abstractmethod
    def file_extension(self) -> str:
        """
        :return: The main file extension for this language, sans the dot.
        """
        raise NotImplementedError

    def is_source_file(self, file: Path) -> bool:
        """
        Check if the given file could be a source file for this programming language.

        Note that this check can be pretty basic: checking the file's extension should
        be enough.

        :param file: Path to the file to check.
        :return: True if the file is a valid source file.
        """
        return file.suffix == f".{self.file_extension()}"

    def with_extension(self, file_name: str) -> str:
        """
        :return: The file name with the language's extension appended to it.
        """
        return f"{file_name}.{self.file_extension()}"

    def submission_file(self) -> str:
        """
        :return: The name of the submission.
        """
        return self.with_extension(submission_name(self))

    @abstractmethod
    def initial_dependencies(self) -> list[str]:
        """
        Return the additional dependencies that tested will include in compilation.
        The dependencies are read from the config.json file.

        :return: A list of dependencies, relative to the "templates" folder.
        """
        raise NotImplementedError

    @abstractmethod
    def needs_selector(self) -> bool:
        """
        Return if the language needs a selector for batch compilation or not.

        :return: True if a selector is needed, false otherwise.
        """
        raise NotImplementedError

    def supports_debug_information(self) -> bool:
        """
        If the language supports Dodona debug information for the Python tutor.
        :return: True if yes, false otherwise.
        """
        return False

    def supported_constructs(self) -> set[Construct]:
        """
        Callback to get the supported constructs for a language. By default, no
        features are returned, i.e. the default is false.

        :return: The features supported by this language.
        """
        return set()

    def map_type_restrictions(self) -> set[ExpressionTypes] | None:
        """
        Get type restrictions that apply to map types in this language.

        If you return None, all data types are assumed to be usable as the key in
        a map data type, such as a dictionary or hashmap. Otherwise, you must return
        a whitelist of the allowed types.

        :return: The whitelist of allowed types, or everything is allowed.
        """
        return None

    def set_type_restrictions(self) -> set[ExpressionTypes] | None:
        """
        Get type restrictions that apply to the set types in this language.

        If you return None, all data types are assumed to be usable as the key in
        a set data type, such as a HashSet. Otherwise, you must return a whitelist
        of the allowed types.

        :return: The whitelist of allowed types, or everything is allowed.
        """
        return None

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        """
        Override support for datatypes.

        By default, all types are unsupported. By overriding this, you can indicate
        support for more types.

        :return: A dictionary of types mapped to their support level.
        """
        return dict()

    def modify_solution(self, solution: Path):
        """
        An opportunity to modify the solution. By default, this does nothing.
        If you modify the solution, you must overwrite the contents of the solution
        in-place.

        This callback is called after linting, but before any compilation.

        :param solution: Path to the solution and path for the modified solution.
        """
        pass

    def compiler_output(
        self, stdout: str, stderr: str
    ) -> tuple[list[Message], list[AnnotateCode], str, str]:
        """
        Convert the stdout and stderr from a compiler to more structured data.

        Note that you should not clean stacktraces; this is done automatically
        for the returned stdout and stderr.

        :param stdout: The standard output from the compiler.
        :param stderr: The standard error from the compiler.
        :return: A tuple containing:
                 - A list of messages to show to the user.
                 - A list of annotations to add.
                 - The new stdout and stderr.
        """
        return [], [], stdout, stderr

    def linter(self, remaining: float) -> tuple[list[Message], list[AnnotateCode]]:
        """
        Run a linter or other code analysis tools on the submission.
        The messages that are output will be passed to Dodona.
        By default, this does nothing.

        Note that you should not modify the solution file. There is no guarantee
        that this solution is the one that will be evaluated.

        :param remaining: The time the judge can use.

        :return: A list of messages and annotations.
        """
        return [], []

    def filter_dependencies(self, files: list[Path], context_name: str) -> list[Path]:
        """
        Callback to filter dependencies for one context.

        These dependencies are the result of the compilation step. Only the files
        accepted by this filter are available in the execution step.

        By default, all non-context files are accepted, in addition to the files for
        the current context.

        :param files: The files resulting from the compilation step(s).
        :param context_name: The name of the current context.
        :return: A list of filtered files.
        """

        def filter_function(file: Path) -> bool:
            # We don't want files for contexts that are not the one we use.
            prefix = conventionalize_namespace(self, EXECUTION_PREFIX)
            is_context = file.name.startswith(prefix)
            is_our_context = file.name.startswith(context_name + ".")
            return not is_context or is_our_context

        return list(x for x in files if filter_function(x))

    def find_main_file(
        self,
        files: list[Path],
        name: str,
    ) -> Path | Status:
        """
        Find the "main" file in a list of files.

        This method is invoked to find the "main" file, i.e. the file with the main
        method (or at least the file that should be executed).

        If something went wrong, an error status is returned. Otherwise, a path to
        the main file is returned.

        :param files: A list of files.
        :param name: The name of the main file.
        :return: The main file or an error status.
        """
        _logger.debug("Finding %s in %s", name, files)
        possible_main_files = [x for x in files if x.name.startswith(name)]
        if possible_main_files:
            return possible_main_files[0]
        else:
            return Status.COMPILATION_ERROR

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        """
        Clean up a stacktrace.

        The language implementation should, if possible, remove all references to
        "internal" TESTed code, and also replace all paths to the submission with
        the placeholder "<code>".

        :param stacktrace: Stack trace to clean up.
        :return A clean stack trace
        """
        return stacktrace

    def cleanup_description(self, statement: str) -> str:
        """
        Allow the language implementation to modify a generated statement for use
        in problem statements.

        :param statement: The generated statement.
        """
        return statement

    @abstractmethod
    def generate_statement(self, statement: Statement) -> str:
        """
        Generate code for a (prepared) statement.

        :param statement: The prepared statement.
        :return: A string representing the statement.
        """
        raise NotImplementedError

    @abstractmethod
    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        """
        Generate code for a prepared execution unit.

        :param execution_unit: The prepared execution unit.
        :return: A string representing the execution unit.
        """
        raise NotImplementedError

    def generate_selector(self, contexts: list[str]) -> str:
        """
        Generate code for a selector for the given list of contexts.

        :param contexts: The contexts the selector must support.
        :return: A string representing the selector.
        """
        raise NotImplementedError

    @abstractmethod
    def generate_encoder(self, values: list[Value]) -> str:
        """
        Generate code for a main function that will encode the given values.

        Note that this is only used in the tests, not in production.

        :param values: The values to encode.
        :return: A string representing an encoder.
        """
        raise NotImplementedError

    @abstractmethod
    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        """
        Return metadata that can be used to construct type declarations.

        In languages that support it, this declaration should be valid to use on a
        variable, for example. In other languages, an approximation can be used.
        """
        raise NotImplementedError

    def path_to_dependencies(self) -> list[Path]:
        """
        Construct the paths to the folder containing the additional dependencies
        needed for a programming language.

        :return: A list of template folders.
        """
        assert self.config
        lang = self.config.dodona.programming_language
        return [self.config.dodona.judge / "tested" / "languages" / lang / "templates"]

    def is_void_method(self, name: str) -> bool:
        """
        Check if a function with a name returns nothing.
        """
        return False
