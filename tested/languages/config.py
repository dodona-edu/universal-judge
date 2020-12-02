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
from collections import defaultdict
from dataclasses import dataclass
from enum import Enum, auto
from pathlib import Path
from typing import List, Tuple, Mapping, Union, Callable, Set, Dict, Optional, Any

from ..configs import Bundle
from ..datatypes import AllTypes
from ..dodona import AnnotateCode, Message, Status, ExtendedMessage, Permission
from ..features import Construct
from ..serialisation import ExceptionValue
from ..testplan import Plan
from ..utils import camelize, pascalize, fallback, snake_case

Command = List[str]
FileFilter = Callable[[Path], bool]
CallbackResult = Tuple[Command, Union[List[str], FileFilter]]
logger = logging.getLogger(__name__)

_case_mapping = {
    "camel_case":  camelize,
    "pascal_case": pascalize,
    "snake_case":  snake_case
}


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
            options=bundle.config.config_for()
        )


def _conventionalize(options: dict, what: str, name: str):
    """Conventionalize based on the options."""
    function = _case_mapping[
        options.get("naming_conventions", {}).get(what, "snake_case")]
    return function(name)


def executable_name(basename: str) -> str:
    """
    Utility function that will

    :param basename: The name of the executable without extension.

    :return: The executable with extension corresponding to the platform.
    """
    if os.name == 'nt':
        return f"{basename}.exe"
    else:
        return basename


def limit_output(output: str,
                 limit_characters: int = 512,
                 max_lines: int = 20,
                 ellipsis_str: str = '...') -> str:
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
            forward_buffer.append(lines[f][:(max_chars - 1)])
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
                    pass
                # Both lines needed abbreviation
                else:
                    forward_buffer.append(next_line[:math.ceil(half - 1)])
                    backward_buffer.append(prev_line[-math.floor(half - 1):])
                # Terminate loop because character limit reached
                break
    # Concat buffer
    return '\n'.join(forward_buffer + [ellipsis_str] + backward_buffer[::-1])


def trace_to_html(traceback: str,
                  link_regex: str = r'&lt;code&gt;:([0-9]+)',
                  link_subs: str = r'<a href="#" class="tab-link" data-tab="code" '
                                   r'data-line="\1">&lt;code&gt;:\1</a>'
                  ) -> ExtendedMessage:
    # Escape special characters
    traceback = html.escape(traceback)
    # Compile regex
    link_regex = re.compile(link_regex)
    # Add links to
    traceback = link_regex.sub(link_subs, traceback)
    logger.debug(f'<pre><code>{traceback}</code></pre>')
    return ExtendedMessage(
        description=f'<pre><code>{traceback}</code></pre>',
        format="html",
        permission=Permission.STUDENT
    )


class TypeSupport(Enum):
    SUPPORTED = auto()
    UNSUPPORTED = auto()
    REDUCED = auto()


class TemplateType(str, Enum):
    STATEMENT = "statement"
    CONTEXT = "context"
    SELECTOR = "selector"
    EVALUATOR_EXECUTOR = "evaluator_executor"


class Language:
    """
    Base configuration class for a programming language.

    When you need to override a function, check the docs or the implementation to
    see if the function reads configuration data from the config.json file. If this
    is the case, it is recommended to modify the value in the config.json file
    instead of overriding the function in a subclass.
    """
    __slots__ = ["options"]

    def __init__(self, config_file: str = "config.json"):
        """
        Initialise a language configuration. Subclasses can modify the name of the
        toml configuration file. By default, a "config.json" file is expected in the
        same directory as the configuration class.

        :param config_file: The name of the configuration file. Relative to the
                            directory in which the configuration class is.
        """
        path_to_config = (Path(sys.modules[self.__module__].__file__).parent
                          / config_file)
        with open(path_to_config, "r") as f:
            self.options = json.load(f)

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
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

        The callback functions receives a filename as argument.

        Non-compiling languages
        -----------------------
        Languages that do not need compilation must return an empty list as
        compilation command, and can return the ``files`` unchanged. This is also
        the default implementation of this function.

        Parameters
        ----------

        :param config: Various configuration options.
        :param files: A suggestion containing the dependencies TESTed thinks might
                      be useful to compile. By convention, the last file in the list
                      is the file containing the "main" function.

        :return: The compilation command and either the resulting files or a filter
                 for the resulting files.
        """
        return [], files

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        """
        Callback for generating the execution command.

        The execution command must execute the file given by the ``file`` parameter.
        When executing the file, the ``arguments`` MUST be passed to the programme.
        The returned command will be passed to the subprocess module.

        The ``cwd`` parameter is mainly useful for executables. Since those are not
        on the PATH, you should use an absolute path to those instead of a relative
        one.

        :param config: Various configuration options. 
        :param cwd: The directory in which the ``file`` is.
        :param file: The file to execute.
        :param arguments: Arguments that must be passed to the execution.

        :return: The execution command.
        """
        raise NotImplementedError

    def conventionalize_function(self, function: str) -> str:
        """
        Conventionalize the name of a function. This function uses the format
        specified in the config.json file. If no format is specified, the function
        name is unchanged, which is the same as snake_case, since the testplan uses
        snake case.

        :param function: The name of the function to conventionalize.
        :return: The conventionalized function.
        """
        return _conventionalize(self.options, "function", function)

    def conventionalize_namespace(self, namespace: str) -> str:
        """
        Conventionalize the name of a namespace (class/module). This function uses
        the format specified in the config.json file. If no format is specified, the
        function name is unchanged, which is the same as snake_case, since the
        testplan uses snake case.

        :param namespace: The name of the namespace to conventionalize.
        :return: The conventionalized namespace.
        """
        return _conventionalize(self.options, "namespace", namespace)

    def submission_name(self, plan: Plan) -> str:
        """
        Get the namespace (module/class) for the submission. This will use the
        namespace specified in the testplan. The name is conventionalized for the
        programming language.

        :param plan: The testplan we are executing.
        :return: The name for the submission, conventionalized.
        """
        return self.conventionalize_namespace(plan.namespace)

    def selector_name(self) -> str:
        """
        :return: The name for the selector, conventionalized.
        """
        return self.conventionalize_namespace("selector")

    def context_prefix(self) -> str:
        """The "name" or prefix for the context names. Not conventionalized."""
        return "context"

    def context_name(self, tab_number: int, context_number: int) -> str:
        """
        Get the name of a context. The name should be unique for the tab and context
        number combination.

        :param tab_number: The number of the tab.
        :param context_number: The number of the context.
        :return: The name of the context, conventionalized.
        """
        return self.conventionalize_namespace(
            f"{self.context_prefix()}_{tab_number}_{context_number}"
        )

    def extension_file(self) -> str:
        """
        The main file extension for this language, sans the dot. This is read from
        the config.json file.
        """
        return self.options["extensions"]["file"]

    def with_extension(self, file_name: str) -> str:
        """Utility function to append the file extension to a file name."""
        return f"{file_name}.{self.extension_file()}"

    def extension_templates(self) -> List[str]:
        """
        A list of extensions for the template files. By default, this uses the
        ``extension_file`` and ``mako``.

        :return: The templates.
        """
        default = [self.extension_file(), "mako"]
        return self.options.get("extensions").get("templates", default)

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
        raw_config: Dict[str, str] = self.options.get("datatypes", {})
        config = {x: TypeSupport[y.upper()] for x, y in raw_config.items()}
        return fallback(defaultdict(lambda: TypeSupport.UNSUPPORTED), config)

    def solution(self, solution: Path, bundle: Bundle):
        """
        An opportunity to modify the solution. By default, this does nothing.
        If you modify the solution, you must overwrite the contents of the solution
        in-place.

        This callback is called after linting, but before any compilation.

        :param solution: Path to the solution and path for the modified solution.
        :param bundle: The configuration bundle.
        """
        pass

    def specific_evaluator(self, evaluator: Path, bundle: Bundle):
        """
        An opportunity to modify the language specific evaluator. By default,
        this does nothing. If you modify the evaluator, you must overwrite the
        contents of the evaluator in-place.

        This callback is called before any compilation.

        :param evaluator: Path to the evaluator and path for the modified evaluator.
        :param bundle: The configuration bundle.
        """
        pass

    def compiler_output(
            self, namespace: str, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        """
        Callback that allows processing the output of the compiler. This might be
        useful to filter TESTed code or add links to the code tab on Dodona.

        TODO: in context compilation mode, this is called for each compilation,
          which can result in the same annotation 50x times.

        :param namespace: Namespace of the test plan
        :param stdout: The standard output from the compiler.
        :param stderr: The standard error from the compiler.
        :return: A tuple containing:
                 - A list of messages to show to the user.
                 - A list of annotations to add.
                 - The new stdout and stderr.
        """
        return [], [], limit_output(stdout), limit_output(stderr)

    def exception_output(self,
                         bundle: Bundle,
                         exception: ExceptionValue) -> ExceptionValue:
        """
        Callback that allows modifying the exception value, for example the
        stacktrace.

        :param bundle: Evaluation bundle
        :param exception: The exception.
        :return: The modified exception.
        """
        namespace = self.conventionalize_namespace(bundle.plan.namespace)
        exception.stacktrace = self.cleanup_stacktrace(
            exception.stacktrace, self.with_extension(namespace)
        )
        exception.message = self.clean_exception_message(exception.message,
                                                         namespace)
        return exception

    def stdout(self,
               _bundle: Bundle,
               stdout: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        """
        Callback that allows modifying the stdout.

        :param _bundle: Gives context to the execution stdout
        :param stdout: The original stdout.
        :return: A tuple containing messages, annotations and the new stdout.
        """
        return [], [], stdout

    def stderr(self,
               bundle: Bundle,
               stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        """
        Callback that allows modifying the stderr.

        :param bundle: Gives context to the execution stderr
        :param stderr: The original stderr.
        :return: A tuple containing messages, annotations and the new stderr.
        """
        return [], [], stderr

    def linter(self, bundle: Bundle, submission: Path, remaining: float) \
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

    def template_name(self, template_type: Union[TemplateType, str]) -> str:
        """
        Get the name for built-in templates. This can be specified in the
        config.json file, but needing to override this is generally not necessary.

        :param template_type: The built-in type.
        :return: The name of the template (without extension).
        """
        return self.options.get("templates", {}).get(template_type, template_type)

    def inherits_from(self) -> Optional[str]:
        """
        Indicates that this language inherits from another language. This means
        that the other language will be used as a fallback if something is not
        implemented in the other language (templates only, if you need to inherit
        this class, just extend the config class the language).

        :return: An optional language.
        """
        return self.options["general"].get("inherits")

    def filter_dependencies(self,
                            bundle: Bundle,
                            files: List[str],
                            context_name: str) -> List[str]:
        def filter_function(file: str) -> bool:
            # We don't want files for contexts that are not the one we use.
            prefix = bundle.lang_config.conventionalize_namespace(
                bundle.lang_config.context_prefix()
            )
            is_context = file.startswith(prefix)
            is_our_context = file.startswith(context_name + ".")
            return not is_context or is_our_context

        return list(x for x in files if filter_function(x))

    def find_main_file(self, files: List[str], name: str) \
            -> Tuple[Optional[str], List[Message], Status, List[AnnotateCode]]:
        logger.debug("Finding %s in %s", name, files)
        messages = []
        possible_main_files = [x for x in files if x.startswith(name)]
        if possible_main_files:
            return possible_main_files[0], messages, Status.CORRECT, []
        else:
            messages.append("Unknown compilation error")
            return None, messages, Status.COMPILATION_ERROR, []

    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
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
