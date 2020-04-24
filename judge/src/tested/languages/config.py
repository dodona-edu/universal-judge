"""
The configuration class for a programming language in TESTed. Note that this is one
of the three things you must implement:

- This class
- A config.toml file
- The templates which are used to create the code.

## Implementing a new programming language

The first thing you should do is implement this class and the accompanying toml
file. While the toml file is not strictly required, it makes implementing the
configuration class a lot less work. You can still override all functions and do
something special instead of reading it from the toml file.

There are a few callbacks that must be implemented. These raise a
`NotImplementedError`, so proper editors will warn you (or TESTed will crash when
using your language).
"""
import os
import sys
from collections import defaultdict
from enum import Enum, auto
from pathlib import Path
from typing import List, Tuple, Mapping, Union, Callable, Set, Dict

import toml

from ..configs import Bundle
from ..datatypes import AllTypes
from ..dodona import AnnotateCode, Message
from ..features import Construct
from ..serialisation import ExceptionValue
from ..testplan import Plan
from ..utils import camelize, pascalize, fallback

Command = List[str]
CallbackResult = Tuple[Command, Union[List[str], Callable[[Path, str], bool]]]

_case_mapping = {
    "camel_case":  camelize,
    "pascal_case": pascalize,
    "snake_case":  (lambda x: x)
}


def _conventionalize(options: dict, what: str, name: str):
    """Conventionalize based on the options."""
    function = _case_mapping[options.get("formats", {}).get(what, "snake_case")]
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


class TypeSupport(Enum):
    SUPPORTED = auto()
    UNSUPPORTED = auto()
    REDUCED = auto()


class TemplateType(str, Enum):
    STATEMENT = "statement"
    CONTEXT = "context"
    SELECTOR = "selector"
    VALUE = "value"
    EVALUATOR_EXECUTOR = "evaluator_executor"


class Language:
    """
    Base configuration class for a programming language. The functions in this class
    follow the following convention:

    - Functions prefixed with `c_` are intended to be overridden in the
      configuration class of your programming language (or at least provide the
      possibility; in most cases the defaults are enough). The c stands for
      "callback".
    - Functions prefixed with `p_` are intended to be read from the toml
      file. You can override them, but it is recommended to adjust your toml file
      instead. The p stans for "property".
    """
    __slots__ = ["options"]

    def __init__(self, config_file: str = "config.toml"):
        """
        Initialise a language configuration. Subclasses can modify the name of the
        toml configuration file. By default, "config.toml" file is expected in the
        same directory as the configuration class.

        :param config_file: The name of the configuration file. Relative to the
                            directory in which the configuration class is.
        """
        path_to_config = (Path(sys.modules[self.__module__].__file__).parent
                          / config_file)
        self.options = toml.load(path_to_config)

    def c_compilation(self, files: List[str]) -> CallbackResult:
        """
        Callback for generating the compilation command.

        Files
        -----

        The files parameter contains the dependencies which TESTed assumes can be
        useful for compilation. This generally includes the `dependencies` from the
        toml config, the submission and either the context or the context and the
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

        The callback functions receives two arguments: a file and the name of the
        current context. The filter function will be called for each file in the
        directory after compilation and before each context is executed. This allows
        fine-grained control over which files are needed and which files are not.
        Some examples what can be done with this callback function:

        - In languages such as Java, one .java file might result in multiple .class
          files. In that case we need to include all of those.
        - In Java and Python, the result of the compilation step is a lot of files,
          i.e. one compiled file per context. However, when executing, only the
          files for one context are needed. With the context name, those can be
          filtered as well.

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

    def c_execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
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

    def c_conventionalize_function(self, function: str) -> str:
        """
        Conventionalize the name of a function. By default this uses the format
        specified in the toml file called "function" The default implementation is
        snake_case.

        :param function: The name of the function to conventionalize.
        :return: The conventionalized function.
        """
        return _conventionalize(self.options, "function", function)

    def c_conventionalize_namespace(self, namespace: str) -> str:
        """
        Conventionalize the name of a namespace (class/module). By default this uses
        the format from the toml file called "namespace". The default implementation
        is snake_case.

        :param namespace: The name of the namespace to conventionalize.
        :return: The conventionalized namespace.
        """
        return _conventionalize(self.options, "namespace", namespace)

    def c_submission_name(self, plan: Plan) -> str:
        """
        Get the namespace (module/class) for the submission.

        :param plan: The testplan we are executing.
        :return: The name for the submission, not conventionalized.
        """
        return self.c_conventionalize_namespace(plan.namespace)

    def c_selector_name(self) -> str:
        """
        :return: The name for the selector, not conventionalized.
        """
        return self.c_conventionalize_namespace("selector")

    def c_context_name(self, tab_number: int, context_number: int) -> str:
        """
        Get the name of a context. The name should be unique for the tab and context
        number combination.

        :param tab_number: The number of the tab.
        :param context_number: The number of the context.
        :return: The name of the context, not conventionalized for the language.
        """
        return self.c_conventionalize_namespace(
            f"context_{tab_number}_{context_number}"
        )

    def p_extension_file(self) -> str:
        """The main file extension for this language, sans the dot."""
        return self.options["extensions"]["file"]

    def with_extension(self, file_name: str) -> str:
        """Utility function to append the file extension to a file name."""
        return f"{file_name}.{self.p_extension_file()}"

    def template_extensions(self) -> List[str]:
        """Extensions a template can be in."""
        return [self.p_extension_file(), "mako"]

    def p_extension_templates(self) -> List[str]:
        """
        A list of extensions for the template files. By default, this uses the
        ``p_extension_file`` and ``mako``.

        :return: The templates.
        """
        default = [self.p_extension_file(), "mako"]
        return self.options.get("extension").get("templates", default)

    def p_initial_dependencies(self) -> List[str]:
        """
        Return the additional dependencies that tested will include in compilation.

        :return: A list of dependencies, relative to the "templates" folder.
        """
        return self.options["general"]["dependencies"]

    def p_needs_selector(self):
        """
        :return: True if a selector is needed, false otherwise.
        """
        return self.options["general"]["selector"]

    def c_supported_constructs(self) -> Set[Construct]:
        """
        Callback to get the supported constructs for a language. By default, all
        features are returned.

        Languages can declare missing support for features in one of two ways:

        - Enumerating all supported features. This is safe against new features.
        - Explicitly disallowing some features. This also means the language will
          automatically support new features when they are added to TESTed.

        :return: The features supported by this language.
        """
        config: Dict[str, bool] = self.options.get("constructs", {})
        result = {x for x in Construct}
        for construct, supported in config.items():
            if not supported:
                result.remove(Construct[construct.upper()])
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
        return fallback(defaultdict(lambda: TypeSupport.REDUCED), config)

    def c_solution(self, solution: Path, bundle: Bundle):
        """
        An opportunity to modify the solution. By default, this does nothing.
        If you modify the solution, you must overwrite the contents of the solution
        in-place.

        This callback is called after linting, but before any compilation.

        :param solution: Path to the solution and path for the modified solution.
        :param bundle: The configuration bundle.
        """
        pass

    def c_specific_evaluator(self, evaluator: Path, bundle: Bundle):
        """
        An opportunity to modify the language specific evaluator. By default,
        this does nothing. If you modify the evaluator, you must overwrite the
        contents of the evaluator in-place.

        This callback is called before any compilation.

        :param evaluator: Path to the evaluator and path for the modified evaluator.
        :param bundle: The configuration bundle.
        """
        pass

    def c_compiler_output(
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
        return [], [], stdout, stderr

    def c_exception_output(self, exception: ExceptionValue) -> ExceptionValue:
        """
        Callback that allows modifying the exception value, for example the
        stacktrace.

        :param exception: The exception.
        :return: The modified exception.
        """
        return exception

    def c_stdout(self,
                 stdout: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        """
        Callback that allows modifying the stdout.

        :param stdout: The original stdout.
        :return: A tuple containing messages, annotations and the new stdout.
        """
        return [], [], stdout

    def c_stderr(self,
                 stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        """
        Callback that allows modifying the stderr.

        :param stderr: The original stderr.
        :return: A tuple containing messages, annotations and the new stderr.
        """
        return [], [], stderr

    # noinspection PyUnusedLocal
    def c_linter(self, bundle: Bundle, submission: Path, remaining: float) \
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

    def c_template_name(self, template_type: TemplateType) -> str:
        """
        Get the name for built-in templates.

        :param template_type: The built-in type.
        :return: The name of the template (without extension).
        """
        return self.options.get("templates", {}).get(template_type, template_type)
