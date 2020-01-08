"""Code generators for the testplans."""
import copy
import random
import string
import tempfile
from dataclasses import dataclass, replace

import shutil
import subprocess
from functools import cached_property
from mako.exceptions import TemplateLookupException
from mako.lookup import TemplateLookup
from pathlib import Path
from typing import Tuple, List, Optional

from dodona import ExtendedMessage
from runners.config import LanguageConfig
from runners.dependencies import Destination, File, Dependencies
from runners.haskell import HaskellConfig
from runners.java import JavaConfig
from runners.jshell import JshellConfig
from runners.python import PythonConfig
from runners.templates import write_template, ContextData, TestcaseData, EvaluatorData, CustomData, \
    MainTestcaseData
from runners.utils import remove_indents, remove_newline
from serialisation import Value
from tested import Config
from testplan import Context, FunctionCall, SpecificEvaluator, IgnoredChannelState, NoneChannelState, \
    TestPlanError, NoMainTestcase, Testcase, NormalTestcase, TextData, MainTestcase, Plan


def _get_identifier() -> str:
    """Generate a random identifier valid in most languages."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


@dataclass
class BaseExecutionResult:
    """
    Base result of a testcase execution.
    """
    stdout: str
    stderr: str
    exit: int  # The return value.


@dataclass
class ExecutionResult(BaseExecutionResult):
    """
    The result of a main testcase execution.

    All output streams are divided per testcase, in the same order as the context that was used to
    execute the test. E.g. the string at position 0 in stdout is the result of executing the
    testcase at position 0 in the context.
    """
    separator: str
    results: str
    exceptions: str


class BaseRunner:
    """
    Base runner to test a submission in a given programming language.

    For most language, it is enough to define a :class:`LanguageConfig`, and use the pre-made
    :class:`ConfigurableRunner`.

    The lifecycle is basically as follows:

    - :func:`pre_execute` is called, allowing for common things to be prepared.
    - The main method is :func:`execute`, which will execute the tests for one context, and report
      back the results.
    """

    def __init__(self, config: Config):
        self.config = config

    def pre_execute(self, working_directory: Path, plan: Plan) -> Optional[BaseExecutionResult]:
        """
        Prepare execution. This is the moment to compile common files.

        :param plan: The testplan.
        :param working_directory: The working directory.
        :return: The result or None if nothing was done.
        """
        raise NotImplementedError

    def execute(self,
                plan: Plan,
                context: Context,
                common_path: Path,
                working_directory: Path) -> Tuple[BaseExecutionResult, Optional[ExecutionResult]]:
        """
        Execute the tests for a given context. This function typically implements following steps:

        1. Generate the code necessary for executing the tests for this context.
        2. (Optional) Compile the code.
        3. Run the code and report back the result.

        Note that this method should be thread-safe: multiple context may be evaluated in parallel.

        :param common_path: The common path.
        :param plan: The test plan.
        :param working_directory: The working directory in which the context must be executed. While
               read access outside this directory is not a problem, write access is not guaranteed.
        :param context: The context that must be evaluated.
        :return: A tuple containing the compiler results and optionally the main results. If
                 the compilation was not successful (exit code non 0), the main results will
                 not be available. Else the result of the main, which allows judging of the
                 results.
        """
        raise NotImplementedError

    def function_call(self, submission_name: str, call: FunctionCall) -> str:
        """Create a textual representation of a function call, to display to the user."""
        raise NotImplementedError

    def evaluate_specific(self, code: str, expected: Value, actual: Value) -> BaseExecutionResult:
        """Evaluate two values with code provided by the test plan."""
        raise NotImplementedError

    def get_readable_input(self, plan: Plan, context: Context, case: Testcase) -> ExtendedMessage:
        """Get the input for a testcase. See the implementations for more details on what is used."""
        raise NotImplementedError


class ConfigurableRunner(BaseRunner):
    """Base runner working with language configurations."""

    def __init__(self, config: Config, language_config: LanguageConfig):
        super().__init__(config)
        self.language_config = language_config
        self.identifier = _get_identifier()
        self.files = Dependencies(
            [File(x, Destination.PRE_COMPILATION) for x in language_config.initial_dependencies()]
        )

    def _write_context_template(self,
                                plan: Plan,
                                context: Context,
                                submission_name: str,
                                destination: Path,
                                dependencies: Dependencies):
        """
        Write the context template to file.

        The parameters of this function are available to the template. Note that just because they
        are available, does not mean they need to be used. For example, the before and after should
        be executed where it makes sense for the language. For example, in Python, this is in the
        module of the user's code, but in Java, this is in the main of the context.

        :param context: The context to execute.
        :param submission_name: The name of the container for the submitted code. For example, in
                                Java this will be the class, in Haskell the module.
        :param destination: The path where the files should be generated.
        :return: The files (base names) that were generated, in order of dependencies.
        """
        # The code for before and after the context.
        before_code = context.before.get(self.config.programming_language, "")
        after_code = context.after.get(self.config.programming_language, "")

        context_template = self._find_template("context", self._get_environment)

        value_file = str(self._value_file(destination)).replace("\\", "/")
        exception_file = str(self._exception_file(destination)).replace("\\", "/")

        additional_testcases = self._get_additional_testcases(plan, context)
        main_testcase = self._get_main_testcase(context)

        data = ContextData(
            secret_id=self.identifier,
            value_file=value_file,
            exception_file=exception_file,
            submission_name=submission_name,
            before=before_code,
            after=after_code,
            main_testcase=main_testcase,
            additional_testcases=additional_testcases,
        )
        write_template(data, context_template, destination / self._context_name())

        evaluator_template = self._find_template("evaluators", self._get_environment)
        evaluator_data = EvaluatorData(
            main_testcase=main_testcase,
            additional_testcases=additional_testcases,
            value_file=value_file,
            exception_file=exception_file
        )
        write_template(evaluator_data, evaluator_template, destination / self._evaluator_name())

        dependencies.add([
            File(self._evaluator_name(), Destination.COMPILATION),
            File(self._context_name(), Destination.COMPILATION)
        ])

    def _generate_code(self,
                       plan: Plan,
                       context: Context,
                       working_directory: Path,
                       common_path: Path,
                       dependencies: Dependencies):
        """Generate the code needed to test one context."""
        submission_name = self.language_config.submission_name(plan)
        # print(f"Files for context are {dependencies}")
        dependencies.copy_to([common_path], working_directory)
        self._write_context_template(plan, context, submission_name, working_directory, dependencies)

    def _value_file(self, working_directory: Path):
        return working_directory / f"{self.identifier}_values.txt"

    def _exception_file(self, working_directory: Path):
        return working_directory / f"{self.identifier}_exceptions.txt"

    def _compile(self, command: List[str], working_directory) -> Optional[BaseExecutionResult]:
        if command:
            p = subprocess.run(command, text=True, capture_output=True, cwd=working_directory)
            return BaseExecutionResult(p.stdout, p.stderr, p.returncode)
        else:
            return None

    def _context_name(self) -> str:
        return f"{self.language_config.context_name()}.{self.language_config.file_extension()}"

    def _evaluator_name(self) -> str:
        return f"{self.language_config.evaluator_name()}.{self.language_config.file_extension()}"

    def pre_execute(self, working_directory: Path, plan: Plan) -> Optional[BaseExecutionResult]:

        # Write the submission file to the correct location.
        submission = self.language_config.submission_name(plan) + "." + self.language_config.file_extension()
        # noinspection PyTypeChecker
        shutil.copy2(self.config.source, working_directory / submission)

        # Copy files to the common directory.
        self.files.copy_to(self._path_to_templates() + [], working_directory)

        self.files.add([File(submission, Destination.PRE_COMPILATION)])

        precompile_files = self.files.get_and_remove(Destination.PRE_COMPILATION)
        command, files = self.language_config.pre_compilation_callback(precompile_files)

        result = self._compile(command, working_directory)

        # Add the files to the dependencies.
        self.files.add([File(x, Destination.COMPILATION) for x in files])

        return result

    def execute(self, plan: Plan,
                context: Context,
                common_path: Path,
                working_directory: Path) -> Tuple[Optional[BaseExecutionResult], Optional[ExecutionResult]]:

        dependencies = copy.deepcopy(self.files)

        # Generate the code from the templates.
        self._generate_code(plan, context, working_directory, common_path, dependencies)

        compilation_files = dependencies.get_and_remove(Destination.COMPILATION)
        command, new_files = self.language_config.compilation_callback(compilation_files)

        # Compile the code. If the language does not need compilation, this should do nothing.
        compile_result = self._compile(command, working_directory)

        dependencies.add([File(x, Destination.EXECUTION) for x in new_files])

        if compile_result is not None and compile_result.exit != 0:
            return compile_result, None

        # Actually execute the testcode.
        stdin_ = []
        for testcase in context.all_testcases():
            if (input_ := testcase.input.get_as_string(self.config.resources)) is not None:
                stdin_.append(input_)
        stdin_ = "\n".join(stdin_)

        execution_files = dependencies.get_and_remove(Destination.EXECUTION)
        command = self.language_config.execution_command(execution_files)
        # noinspection PyTypeChecker
        p = subprocess.run(command, input=stdin_, text=True, capture_output=True, cwd=working_directory)
        identifier = f"--{self.identifier}-- SEP"

        try:
            # noinspection PyTypeChecker
            with open(self._value_file(working_directory), "r") as f:
                values = f.read()
        except FileNotFoundError:
            values = ""

        try:
            # noinspection PyTypeChecker
            with open(self._exception_file(working_directory), "r") as f:
                exceptions = f.read()
        except FileNotFoundError:
            exceptions = ""

        return compile_result, ExecutionResult(p.stdout, p.stderr, p.returncode, identifier, values, exceptions)

    def _path_to_templates(self) -> List[Path]:
        """The path to the templates and normal files."""
        result = []
        for end in self.language_config.template_folders(self.config):
            result.append(Path(self.config.judge) / 'judge' / 'runners' / 'templates' / end)
        return result

    @cached_property
    def _get_environment(self) -> TemplateLookup:
        """Get the environment for the templates."""
        preprocessors = [remove_indents, remove_newline]
        paths = [str(x) for x in self._path_to_templates()]
        return TemplateLookup(directories=paths, preprocessor=preprocessors)

    def _find_template(self, name, environment):
        """Find a template with a name."""
        last_error = None
        for extension in self.language_config.template_extensions():
            try:
                return environment.get_template(f"{name}.{extension}")
            except TemplateLookupException as e:
                last_error = e
        raise last_error

    def function_call(self, submission_name: str, call: FunctionCall) -> str:
        call = self.prepare_function_call(submission_name, call)
        template = self._find_template("function", self._get_environment)
        return template.render(function=call)

    def _get_additional_testcases(self, plan: Plan, context: Context) -> List[TestcaseData]:
        result = []
        testcase: NormalTestcase  # Type hint for PyCharm
        for i, testcase in enumerate(context.normal):
            v_eval_function_name = f"v_evaluate_{i}"
            e_eval_function_name = f"e_evaluate_{i}"
            has_specific_v = not isinstance(testcase.output.result, (IgnoredChannelState, NoneChannelState))
            if has_specific_v and isinstance(testcase.output.result.evaluator, SpecificEvaluator):
                custom_v_code = testcase.output.result.evaluator.evaluators[self.config.programming_language] \
                    .get_data_as_string(self.config.resources)
                custom_v_code = self.language_config.rename_evaluator(custom_v_code, v_eval_function_name)
            else:
                custom_v_code = self.language_config.value_writer(v_eval_function_name)
            custom_e_code = self._get_custom_code(testcase, e_eval_function_name)
            # Convert the function call.
            submission_name = self.language_config.submission_name(plan)
            result.append(TestcaseData(
                function=self.prepare_function_call(submission_name, testcase.input.function),
                stdin=testcase.input.stdin,
                value_code=custom_v_code,
                exception_code=custom_e_code,
                has_return=testcase.output.result != NoneChannelState.NONE
            ))
        return result

    def _get_custom_code(self, testcase: Testcase, function_name: str) -> str:
        """Get the custom code from a context."""
        has_specific = not isinstance(testcase.output.exception, (IgnoredChannelState, NoneChannelState))
        if has_specific and isinstance(testcase.output.exception.evaluator, SpecificEvaluator):
            custom_code = testcase.output.exception.evaluator.evaluators[self.config.programming_language] \
                .get_data_as_string(self.config.resources)
            return self.language_config.rename_evaluator(custom_code, function_name)
        else:
            return self.language_config.exception_writer(function_name)

    def _get_main_testcase(self, context: Context) -> MainTestcaseData:
        if context.main == NoMainTestcase.NONE:
            return MainTestcaseData(exists=False, exception_code="", arguments=[])
        else:
            eval_function_name = f"e_evaluate_main"
            custom_code = self._get_custom_code(context.main, eval_function_name)
            return MainTestcaseData(exists=True, arguments=context.main.input.arguments, exception_code=custom_code)

    def prepare_function_call(self, submission_name: str, function_call: FunctionCall) -> FunctionCall:
        """Prepare the function call for main."""
        object_ = function_call.object or submission_name
        return FunctionCall(
            type=function_call.type,
            arguments=function_call.arguments,
            name=self.language_config.conventionalise(function_call.name),
            object=object_
        )

    def evaluate_specific(self, code: str, expected: Value, actual: Value) -> BaseExecutionResult:
        # Create the template.
        template = self._find_template("evaluator_custom", self._get_environment)
        data = CustomData(evaluator_code=code, expected=expected, actual=actual)
        #directory = Path(self.config.workdir, f"specific")
        #directory.mkdir()
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / self._evaluator_name()
            write_template(data, template, path)
            self.files.copy_to(self._path_to_templates(), directory)
            files = [x.name for x in self.files.files]
            files.append(self._evaluator_name())

            command, _ = self.language_config.compilation_callback(files)

            # Compile the evaluator
            c = self._compile(command, directory)
            if c and c.stderr:
                raise TestPlanError(f"Error while compiling specific test case: {c.stderr}")
            # Execute the evaluator
            command = self.language_config.execute_evaluator("eval")
            p = subprocess.run(command, text=True, capture_output=True, cwd=directory)
            return BaseExecutionResult(p.stdout, p.stderr, p.returncode)

    def get_readable_input(self, plan: Plan, context: Context, case: Testcase) -> ExtendedMessage:
        """
        Get human readable input for a testcase. This function will use, in order of availability:

        1. A description on the testcase.
        2. A function call.
        3. The stdin.
        4. Program arguments, if any.

        :param plan: The testplan.
        :param context: The context of the testcase.
        :param case: The testcase to get the input from.
        """
        format_ = 'text'  # By default, we use text as input.
        if case.description:
            text = case.description
        elif isinstance(case, NormalTestcase) and isinstance(case.input.function, FunctionCall):
            name = plan.object
            text = self.function_call(name, case.input.function)
            format_ = self.config.programming_language
        elif case.input.stdin != NoneChannelState.NONE:
            assert isinstance(case.input.stdin, TextData)
            text = case.input.stdin.get_data_as_string(self.config.resources)
        else:
            assert isinstance(case, MainTestcase)
            if case.input.arguments:
                variable_part = str(case.input.arguments)
            else:
                variable_part = "without arguments"
            text = f"Program main {variable_part}"
        return ExtendedMessage(description=text, format=format_)


CONFIGS = {
    'python': PythonConfig,
    'java': JavaConfig,
    'haskell': HaskellConfig,
    'jshell': JshellConfig
}


def get_runner(config: Config, language: str = None) -> BaseRunner:
    """Get the runner for the specified language."""
    if language is None:
        language = config.programming_language
    adjusted_config = replace(config, programming_language=language)
    return ConfigurableRunner(adjusted_config, CONFIGS[language]())
