"""
The configuration class for a programming language in TESTed. Note that this is one
of the three things you must implement:

- This class
- A config.json file
- The templates which are used to create the code.

Implementing a new programming language
---------------------------------------

The first thing you should do is implement this class and the accompanying toml
file. While the toml file is not strictly required, it makes implementing the
configuration class a lot less work. You can still override all functions and do
something special instead of reading it from the toml file.

There are a few callbacks that must be implemented. These raise a
`NotImplementedError`, so proper editors will warn you (or TESTed will crash when
using your language).
"""
import html
import json
import logging
import math
import os
import re
import sys
import typing
from collections import defaultdict
from dataclasses import dataclass
from enum import Enum, auto
from pathlib import Path
from typing import Any, Callable, Dict, List, Mapping, Optional, Set, Tuple, Union

from tested.configs import Bundle
from tested.datatypes import AdvancedTypes, AllTypes, ExpressionTypes, string_to_type
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Status
from tested.features import Construct
from tested.internationalization import get_i18n_string
from tested.languages.conventionalize import (
    Conventionable,
    NamingConventions,
    conventionalize_namespace,
)
from tested.languages.description_generator import DescriptionGenerator
from tested.serialisation import ExceptionValue, FunctionCall, Statement, Value
from tested.utils import fallback, get_args

if typing.TYPE_CHECKING:
    from tested.configs import GlobalConfig
    from tested.languages.generation import PreparedExecutionUnit

Command = List[str]
FileFilter = Callable[[Path], bool]
CallbackResult = Tuple[Command, Union[List[str], FileFilter]]
logger = logging.getLogger(__name__)

_logger = logging.getLogger(__name__)


@dataclass
class Config:
    # noinspection PyUnresolvedReferences
    """
    Global options for the language configuration.

    Available fields:
    :param time_limit: The time limit as given by Dodona. In most cases, you do not
                       need this; TESTed tracks the execution time.
    :param memory_limit: The memory limit as given by Dodona.
    :param options: Language specific options. Using this field you could for
                    example allow for additional parameters when compiling or
                    executing.
    """
    time_limit: int  # Time limit from Dodona.
    memory_limit: int  # Memory limit from Dodona.
    options: Dict[str, Any]  # Language-specific options.

    @classmethod
    def from_bundle(cls, bundle: Bundle):
        return Config(
            time_limit=bundle.config.time_limit,
            memory_limit=bundle.config.memory_limit,
            options=bundle.config.config_for(),
        )


def executable_name(basename: str) -> str:
    """
    Utility function that will

    :param basename: The name of the executable without extension.

    :return: The executable with extension corresponding to the platform.
    """
    if os.name == "nt":
        return f"{basename}.exe"
    else:
        return basename


def limit_output(
    output: str,
    limit_characters: int = 512,
    max_lines: int = 20,
    ellipsis_str: str = "...",
) -> str:
    """
    Utility function for limiting a string output

    :param output: String that possible needs to be abbreviated
    :param limit_characters: Maximum characters used in the output
    :param max_lines: Maximum lines in the output
    :param ellipsis_str: ellipsis used when abbreviated is needed

    :return: The abbreviated 'output' if needed otherwise the 'output' itself
    """
    lines = output.splitlines()
    # Case character limit not exceeded and line limit not exceeded
    if len(output) <= limit_characters and len(lines) <= max_lines:
        return output
    # Case character limit exceeded
    max_chars = limit_characters - len(ellipsis_str)
    forward_buffer = []
    backward_buffer = []
    len_lines = len(lines)
    for f in range(math.ceil(min(max_lines - 1, len_lines) / 2)):
        r = len_lines - f - 1
        # Case last lines to consider are the same
        if f == r:
            forward_buffer.append(lines[f][: (max_chars - 1)])
        # Otherwise
        else:
            next_line, prev_line = lines[f], lines[r]
            current_length = len(next_line) + len(prev_line) + 2
            # Both lines can be add in full
            if current_length < max_chars:
                forward_buffer.append(next_line)
                backward_buffer.append(prev_line)
                max_chars -= current_length
            # Lines must be limited
            else:
                half = max_chars / 2
                # Next line can be add in full
                if len(next_line) + 2 < max_chars:
                    forward_buffer.append(next_line)
                    max_chars -= len(next_line) + 2
                    backward_buffer.append(prev_line[-max_chars:])
                # Prev line can be add in full
                elif len(prev_line) + 2 < max_chars:
                    backward_buffer.append(prev_line)
                    max_chars -= len(prev_line) + 2
                    forward_buffer.append(next_line[:max_chars])
                # Both lines needed abbreviation
                else:
                    forward_buffer.append(next_line[: math.ceil(half - 1)])
                    backward_buffer.append(prev_line[-math.floor(half - 1) :])
                # Terminate loop because character limit reached
                break
    # Concat buffer
    return "\n".join(forward_buffer + [ellipsis_str] + backward_buffer[::-1])


def trace_to_html(
    traceback: str,
    link_regex: str = r"&lt;code&gt;:([0-9]+)",
    link_subs: str = r'<a href="#" class="tab-link" data-tab="code" '
    r'data-line="\1">&lt;code&gt;:\1</a>',
) -> ExtendedMessage:
    # Escape special characters
    traceback = html.escape(traceback)
    # Compile regex
    link_regex = re.compile(link_regex)
    # Add links to
    traceback = link_regex.sub(link_subs, traceback)
    logger.debug(f"<pre><code>{traceback}</code></pre>")
    return ExtendedMessage(
        description=f"<pre><code>{traceback}</code></pre>",
        format="html",
        permission=Permission.STUDENT,
    )


class TypeSupport(Enum):
    SUPPORTED = auto()
    """
    The type is fully supported.
    
    For advanced types, this requires the language to have a suitable, distinct
    type. It is not enough that another type can support it. For example, Python
    does not have support for int16, even though all int16 values can easily fit
    into the Python integer type.
    """
    UNSUPPORTED = auto()
    """
    There is no support. This is the default value to allow for expansion of the
    types. Exercises which use these types will not be solvable in a language
    for which the type is unsupported.
    """
    REDUCED = auto()
    """
    Used for advanced types only. This means the language has no support for the
    type with a distinct type, but there is support using other types. In this
    case, exercises using this type are still solvable in the programming language.
    TESTed will use the basic type in those languages.
    """


class Language:
    """
    Base configuration class for a programming language.

    When you need to override a function, check the docs or the implementation to
    see if the function reads configuration data from the config.json file. If this
    is the case, it is recommended to modify the value in the config.json file
    instead of overriding the function in a subclass.
    """

    __slots__ = ["options", "config_dir", "_description_generator", "config"]

    # TODO: get rid of the optional here...
    def __init__(
        self, config: Optional["GlobalConfig"], config_file: str = "config.json"
    ):
        """
        Initialise a language configuration. Subclasses can modify the name of the
        toml configuration file. By default, a "config.json" file is expected in the
        same directory as the configuration class.

        :param config_file: The name of the configuration file. Relative to the
                            directory in which the configuration class is.
        """
        self.config = config
        self._description_generator = None
        self.config_dir = Path(sys.modules[self.__module__].__file__).parent
        path_to_config = self.config_dir / config_file
        with open(path_to_config, "r") as f:
            self.options = json.load(f)

    def naming_conventions(self) -> Dict[Conventionable, NamingConventions]:
        """
        Return naming conventions for this language.

        This should return a dictionary containing a mapping of "conventionable"
        items mapped on their naming convention.

        The default for missing conventionable items is snake case.
        :return: A mapping for the naming conventions.
        """
        raise NotImplementedError

    def get_string_quote(self):
        return '"'

    def compilation(self, files: List[str]) -> CallbackResult:
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

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
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

    def execution_prefix(self) -> str:
        """The "name" or prefix for the context names. Not conventionalized."""
        return "execution"

    def execution_name(self, tab_number: int, execution_number: int) -> str:
        """
        Get the name of an execution. The name should be unique for the tab and
        execution number combination.

        :param tab_number: The number of the tab.
        :param execution_number: The number of the execution.
        :return: The name of the context, conventionalized.
        """
        return conventionalize_namespace(
            self, f"{self.execution_prefix()}_{tab_number}_{execution_number}"
        )

    def extension_file(self) -> str:
        """
        The main file extension for this language, sans the dot. This is read from
        the config.json file.
        """
        return self.options["extensions"]["file"]

    def is_source_file(self, file: Path) -> bool:
        """
        Check if the given file is a valid source file

        :param file: File to check if it's a valid source
        :return: If the file is valid source
        """
        return file.suffix == f".{self.extension_file()}"

    def with_extension(self, file_name: str) -> str:
        """Utility function to append the file extension to a file name."""
        return f"{file_name}.{self.extension_file()}"

    def initial_dependencies(self) -> List[str]:
        """
        Return the additional dependencies that tested will include in compilation.
        The dependencies are read from the config.json file.

        :return: A list of dependencies, relative to the "templates" folder.
        """
        return self.options["general"]["dependencies"]

    def needs_selector(self):
        """
        Return if the language needs a selector for batch compilation or not. This
        is a mandatory option in the config.json file.

        :return: True if a selector is needed, false otherwise.
        """
        return self.options["general"]["selector"]

    def supported_constructs(self) -> Set[Construct]:
        """
        Callback to get the supported constructs for a language. By default, no
        features are returned, i.e. the default is false.

        :return: The features supported by this language.
        """
        config: Dict[str, bool] = self.options.get("constructs", {})
        result = set()
        for construct, supported in config.items():
            if supported:
                result.add(Construct[construct.upper()])
        return result

    def type_support_map(self) -> Mapping[AllTypes, TypeSupport]:
        """
        Return a map containing the support for all types.

        See the documentation on the TypeSupport enum for information on how
        to interpret the results.

        Note that it is considered a programming error if a basic type is not
        supported, but the advanced type is supported. This requirement is
        checked by TESTed when using the language.

        :return: The typing support dict.
        """
        raw_config: Dict[str, str] = self.options.get("datatypes", {})
        config = {x: TypeSupport[y.upper()] for x, y in raw_config.items()}
        return fallback(defaultdict(lambda: TypeSupport.UNSUPPORTED), config)

    def restriction_map(self) -> Mapping[AllTypes, Set[ExpressionTypes]]:
        """
        Return a map containing the restrictions for the set type and the map keys
        :return: The restriction dict
        """

        def get_expression_type(type_str: str) -> ExpressionTypes:
            enums = get_args(ExpressionTypes)
            for enum in enums:
                for m in enum.__members__:
                    if type_str == enum[m].value:
                        return enum[m]
            raise ValueError(f"Unknown type string {type_str}")

        raw_config: Dict[str, List[str]] = self.options.get("restrictions", {})
        config = {
            string_to_type("MAP" if x == "map_key" else x.upper()): set(
                (get_expression_type(t) for t in y)
            )
            for x, y in raw_config.items()
        }
        mappings = self.type_support_map()
        for data_type, type_support in mappings.items():
            data_type = get_expression_type(data_type)
            if type_support is TypeSupport.REDUCED and isinstance(
                data_type, get_args(AdvancedTypes)
            ):
                basic_type = data_type.base_type
                for _, restricted in config.items():
                    if basic_type in restricted:
                        restricted.add(data_type)
        return fallback(defaultdict(set), config)

    def modify_solution(self, solution: Path):
        """
        An opportunity to modify the solution. By default, this does nothing.
        If you modify the solution, you must overwrite the contents of the solution
        in-place.

        This callback is called after linting, but before any compilation.

        :param solution: Path to the solution and path for the modified solution.
        """
        pass

    def modify_specific_evaluator(self, evaluator: Path):
        """
        An opportunity to modify the language specific evaluator. By default,
        this does nothing. If you modify the evaluator, you must overwrite the
        contents of the evaluator in-place.

        This callback is called before any compilation.

        :param evaluator: Path to the evaluator and path for the modified evaluator.
        """
        pass

    def compiler_output(
        self, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        """
        Callback that allows processing the output of the compiler. This might be
        useful to filter TESTed code or add links to the code tab on Dodona.

        TODO: in context compilation mode, this is called for each compilation,
          which can result in the same annotation 50x times.

        :param stdout: The standard output from the compiler.
        :param stderr: The standard error from the compiler.
        :return: A tuple containing:
                 - A list of messages to show to the user.
                 - A list of annotations to add.
                 - The new stdout and stderr.
        """
        return [], [], limit_output(stdout), limit_output(stderr)

    def exception_output(
        self, bundle: Bundle, exception: ExceptionValue
    ) -> ExceptionValue:
        """
        Callback that allows modifying the exception value, for example the
        stacktrace.

        :param bundle: Evaluation bundle
        :param exception: The exception.
        :return: The modified exception.
        """
        namespace = conventionalize_namespace(self, bundle.suite.namespace)
        exception.stacktrace = self.cleanup_stacktrace(
            exception.stacktrace, self.with_extension(namespace)
        )
        exception.message = self.clean_exception_message(exception.message, namespace)
        return exception

    def stdout(
        self, _bundle: Bundle, stdout: str
    ) -> Tuple[List[Message], List[AnnotateCode], str]:
        """
        Callback that allows modifying the stdout.

        :param _bundle: Gives context to the execution stdout
        :param stdout: The original stdout.
        :return: A tuple containing messages, annotations and the new stdout.
        """
        return [], [], stdout

    def stderr(
        self, bundle: Bundle, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str]:
        """
        Callback that allows modifying the stderr.

        :param bundle: Gives context to the execution stderr
        :param stderr: The original stderr.
        :return: A tuple containing messages, annotations and the new stderr.
        """
        return [], [], stderr

    def linter(
        self, bundle: Bundle, submission: Path, remaining: float
    ) -> Tuple[List[Message], List[AnnotateCode]]:
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

    def inherits_from(self) -> Optional[str]:
        """
        Indicates that this language inherits from another language. This means
        that the other language will be used as a fallback if something is not
        implemented in the other language (templates only, if you need to inherit
        this class, just extend the config class the language).

        :return: An optional language.
        """
        return self.options["general"].get("inherits")

    def filter_dependencies(
        self, bundle: Bundle, files: List[Path], context_name: str
    ) -> List[Path]:
        def filter_function(file: Path) -> bool:
            # We don't want files for contexts that are not the one we use.
            prefix = conventionalize_namespace(
                bundle.lang_config, bundle.lang_config.execution_prefix()
            )
            is_context = file.name.startswith(prefix)
            is_our_context = file.name.startswith(context_name + ".")
            return not is_context or is_our_context

        return list(x for x in files if filter_function(x))

    def find_main_file(
        self, files: List[Path], name: str, precompilation_messages: List[str]
    ) -> Tuple[Optional[Path], List[Message], Status, List[AnnotateCode]]:
        logger.debug("Finding %s in %s", name, files)
        messages = []
        possible_main_files = [x for x in files if x.name.startswith(name)]
        if possible_main_files:
            return possible_main_files[0], messages, Status.CORRECT, []
        else:
            messages.extend(precompilation_messages)
            messages.append(get_i18n_string("languages.config.unknown.compilation"))
            return None, messages, Status.COMPILATION_ERROR, []

    def cleanup_stacktrace(
        self, traceback: str, submission_file: str, reduce_all=False
    ) -> str:
        """
        Takes a traceback as a string or as a list of strings and returns a reduced
        version of the traceback as a list of strings.

        :param traceback: Stack trace to cleanup
        :param submission_file: Name of the submission file
        :param reduce_all: Option for aggressive cleanup
        :return A clean stack trace
        """
        return traceback

    def cleanup_description(self, namespace: str, description: str) -> str:
        return description

    def clean_stacktrace_to_message(self, stacktrace: str) -> Optional[Message]:
        if stacktrace:
            return trace_to_html(stacktrace)
        else:
            return None

    def clean_exception_message(self, message: str, namespace: str) -> str:
        """
        Cleanup exception message

        :param message: Exception message
        :param namespace: Namespace name
        :return: Processed message
        """
        return message

    def get_description_generator(self) -> DescriptionGenerator:
        if self._description_generator is None:
            self._description_generator = DescriptionGenerator(self, self.config_dir)
        return self._description_generator

    def generate_statement(self, statement: Statement) -> str:
        """
        Generate code for a (prepared) statement.

        :param statement: The prepared statement.
        :return: A string representing the statement.
        """
        raise NotImplementedError

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        """
        Generate code for a prepared execution unit.

        :param execution_unit: The prepared execution unit.
        :return: A string representing the execution unit.
        """
        raise NotImplementedError

    def generate_selector(self, contexts: List[str]) -> str:
        """
        Generate code for a selector for the given list of contexts.

        :param contexts: The contexts the selector must support.
        :return: A string representing the selector.
        """
        raise NotImplementedError

    def generate_check_function(self, name: str, function: FunctionCall) -> str:
        """
        Generate code that calls the given function as a custom check function.

        :param name: The name of the custom check function.
        :param function: The function to call.
        :return: A string representing the custom check function.
        """
        raise NotImplementedError

    def generate_encoder(self, values: List[Value]) -> str:
        """
        Generate code for a main function that will encode the given values.

        Note that this is only used in the tests, not in production.

        :param values: The values to encode.
        :return: A string representing an encoder.
        """
        raise NotImplementedError
