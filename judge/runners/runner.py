"""Code generators for the testplans."""
import random
import shutil
import string
import subprocess
import tempfile
from dataclasses import dataclass, replace
from functools import cached_property
from pathlib import Path
from typing import Tuple, List, Optional

from mako.exceptions import TemplateLookupException
from mako.lookup import TemplateLookup

from dodona import ExtendedMessage
from runners.config import LanguageConfig
from runners.haskell import HaskellConfig
from runners.java import JavaConfig
from runners.javascripted import JavaScriptedConfig
from runners.python import PythonConfig
from runners.templates import write_template, ContextData, TestcaseData, EvaluatorData, CustomData, \
    MainTestcaseData
from runners.utils import remove_indents, remove_newline
from serialisation import Value
from tested import Config
from testplan import Context, FunctionCall, SpecificEvaluator, IgnoredChannelState, NoneChannelState, \
    TestPlanError, NoExecutionTestcase, Testcase, AdditionalTestcase, TextData, ExecutionTestcase


def _get_identifier() -> str:
    """Generate a random identifier valid in most languages."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


@dataclass
class BaseExecutionResult:
    """
    The result of an main_testcase.
    """
    stdout: str
    stderr: str
    exit: int  # The return value.


@dataclass
class ExecutionResult(BaseExecutionResult):
    """
    The result of an main_testcase.

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

    Otherwise, the main method is :func:`execute`, which will execute the tests for one context, and
    report back the results.
    """

    def __init__(self, config: Config):
        self.config = config

    def execute(self,
                context: Context,
                working_directory: Path) -> Tuple[BaseExecutionResult, Optional[ExecutionResult]]:
        """
        Execute the tests for a given context. This function typically implements following steps:

        1. Generate the code necessary for executing the tests for this context.
        2. (Optional) Compile the code.
        3. Run the code and report back the result.

        Note that this method should be thread-safe: multiple context may be evaluated in parallel.

        :param working_directory: The working directory in which the context must be executed. While
               read access outside this directory is not a problem, write access is not guaranteed.
        :param context: The context that must be evaluated.
        :return: A tuple containing the compiler results and optionally the main_testcase results. If
                 the compilation was not successful (exit code non 0), the main_testcase results will
                 not be available. Else the result of the main_testcase, which allows judging of the
                 results.
        """
        raise NotImplementedError

    def function_call(self, submission_name: str, call: FunctionCall) -> str:
        """Create a textual representation of a function call, to display to the user."""
        raise NotImplementedError

    def evaluate_specific(self, code: str, expected: Value, actual: Value) -> BaseExecutionResult:
        """Evaluate two values with code provided by the test plan."""
        raise NotImplementedError

    def get_readable_input(self, context: Context, case: Testcase) -> ExtendedMessage:
        """Get the input for a testcase. See the implementations for more details on what is used."""
        raise NotImplementedError


class ConfigurableRunner(BaseRunner):
    """Base runner working with language configurations."""

    def __init__(self, config: Config, language_config: LanguageConfig):
        super().__init__(config)
        self.language_config = language_config
        self.identifier = _get_identifier()

    def _write_context_template(self,
                                context: Context,
                                submission_name: str,
                                destination: Path) -> List[str]:
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

        additional_testcases = self._get_additional_testcases(context)
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

        return [self._evaluator_name(), self._context_name()]

    def _copy_additional_files(self, destination: Path) -> List[Path]:
        """Copy the additional files to the given directory"""
        files = []
        for file in self.language_config.additional_files():
            origin = self._path_to_templates() / file
            result = destination / file
            # noinspection PyTypeChecker
            shutil.copy2(origin, result)
            files.append(result)
        return files

    def _generate_code(self, context: Context, working_directory: Path) -> List[str]:
        """
        Generate the code needed to test one context.
        :return: A list of generated files (the basename only, not the path).
        """

        # Copy the user's code to our folder.
        submission_name = self.language_config.submission_name(context)
        submission = f"{submission_name}.{self.language_config.file_extension()}"
        destination = working_directory / submission
        self.language_config.create_submission_code(context, self.config.source, destination)

        # All files we've generated, starting with the submission.
        generated_files = [submission]

        # Copy additional files we might need.
        for file in self.language_config.additional_files():
            result = self._path_to_templates() / file
            # noinspection PyTypeChecker
            shutil.copy2(result, working_directory)
            generated_files.append(file)

        context_files = self._write_context_template(context, submission_name, working_directory)
        generated_files.extend(context_files)

        return generated_files

    def _value_file(self, working_directory: Path):
        return working_directory / f"{self.identifier}_values.txt"

    def _exception_file(self, working_directory: Path):
        return working_directory / f"{self.identifier}_exceptions.txt"

    def _compile(self, ordered_files, working_directory) -> BaseExecutionResult:
        command = self.language_config.compilation_command(ordered_files)
        p = subprocess.run(command, text=True, capture_output=True, cwd=working_directory)
        return BaseExecutionResult(p.stdout, p.stderr, p.returncode)

    def _context_name(self) -> str:
        return f"{self.language_config.context_name()}.{self.language_config.file_extension()}"

    def _evaluator_name(self) -> str:
        return f"{self.language_config.evaluator_name()}.{self.language_config.file_extension()}"

    def execute(self,
                context: Context,
                working_directory: Path) -> Tuple[BaseExecutionResult, Optional[ExecutionResult]]:

        # Generate the code we need to execute this context.
        files = self._generate_code(context, working_directory)

        # Compile the code. If the language does not need compilation, this should do nothing.
        compile_result = self._compile(files, working_directory)

        if compile_result.exit != 0:
            return compile_result, None

        # Actually execute the testcode.

        stdin_ = []
        for testcase in context.all_testcases():
            if (input_ := testcase.get_input(self.config.resources)) is not None:
                stdin_.append(input_)
        stdin_ = "\n".join(stdin_)

        command = self.language_config.execution_command()
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

    def _path_to_templates(self) -> Path:
        """The path to the templates and additional files."""
        return Path(self.config.judge) / 'judge' / 'runners' / 'templates' / self.config.programming_language

    @cached_property
    def _get_environment(self) -> TemplateLookup:
        """Get the environment for the templates."""
        path = str(self._path_to_templates())
        preprocessors = [remove_indents, remove_newline]
        return TemplateLookup(directories=[path], preprocessor=preprocessors)

    def _find_template(self, name, environment):
        """Find a template with a name."""
        try:
            return environment.get_template(f"{name}.{self.language_config.file_extension()}")
        except TemplateLookupException:
            return environment.get_template(f"{name}.mako")

    def function_call(self, submission_name: str, call: FunctionCall) -> str:
        call = self.prepare_function_call(submission_name, call)
        template = self._find_template("function", self._get_environment)
        return template.render(function=call)

    def _get_additional_testcases(self, context: Context) -> List[TestcaseData]:
        result = []
        for i, testcase in enumerate(context.additional):
            v_eval_function_name = f"v_evaluate_{i}"
            e_eval_function_name = f"e_evaluate_{i}"
            has_specific_v = not isinstance(testcase.result, (IgnoredChannelState, NoneChannelState))
            if has_specific_v and isinstance(testcase.result.evaluator, SpecificEvaluator):
                custom_v_code = testcase.result.evaluator.evaluators[self.config.programming_language] \
                    .get_data_as_string(self.config.resources)
                custom_v_code = self.language_config.rename_evaluator(custom_v_code, v_eval_function_name)
            else:
                custom_v_code = self.language_config.value_writer(v_eval_function_name)
            has_specific_e = not isinstance(testcase.exception, (IgnoredChannelState, NoneChannelState))
            if has_specific_e and isinstance(testcase.exception.evaluator, SpecificEvaluator):
                custom_e_code = testcase.exception.evaluator.evaluators[self.config.programming_language] \
                    .get_data_as_string(self.config.resources)
                custom_e_code = self.language_config.rename_evaluator(custom_e_code, e_eval_function_name)
            else:
                custom_e_code = self.language_config.exception_writer(e_eval_function_name)
            # Convert the function call.
            submission_name = self.language_config.submission_name(context)
            result.append(TestcaseData(
                function=self.prepare_function_call(submission_name, testcase.function),
                stdin=testcase.stdin,
                value_code=custom_v_code,
                exception_code=custom_e_code,
                has_return=testcase.result != NoneChannelState.NONE
            ))
        return result

    def _get_main_testcase(self, context: Context) -> MainTestcaseData:
        if context.execution == NoExecutionTestcase.NONE:
            return MainTestcaseData(exists=False, exception_code="", arguments=[])
        else:
            eval_function_name = f"e_evaluate_main"
            has_specific = not isinstance(context.execution.exception, (IgnoredChannelState, NoneChannelState))
            if has_specific and isinstance(context.execution.exception.evaluator, SpecificEvaluator):
                custom_code = context.execution.exception.evaluator.evaluators[self.config.programming_language] \
                    .get_data_as_string(self.config.resources)
                custom_code = self.language_config.rename_evaluator(custom_code, eval_function_name)
            else:
                custom_code = self.language_config.exception_writer(eval_function_name)
            return MainTestcaseData(exists=True, arguments=context.execution.arguments, exception_code=custom_code)

    def prepare_function_call(self, submission_name: str, function_call: FunctionCall) -> FunctionCall:
        """Prepare the function call for main_testcase."""
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
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / self._evaluator_name()
            write_template(data, template, path)

            files = self.language_config.additional_files()
            # Copy necessary files to the temporary directory.
            for file in self.language_config.additional_files():
                result = self._path_to_templates() / file
                # noinspection PyTypeChecker
                shutil.copy2(result, directory)

            files.append(self._evaluator_name())

            # Compile the evaluator
            c = self._compile(files, directory)
            if c.stderr:
                raise TestPlanError(f"Error while compiling specific test case: {c.stderr}")
            # Execute the evaluator
            command = self.language_config.execute_evaluator("eval")
            p = subprocess.run(command, text=True, capture_output=True, cwd=directory)
            return BaseExecutionResult(p.stdout, p.stderr, p.returncode)

    def get_readable_input(self, context: Context, case: Testcase) -> ExtendedMessage:
        """
        Get human readable input for a testcase. This function will use, in order of availability:

        1. A description on the testcase.
        2. A function call.
        3. The stdin.
        4. Program arguments, if any.

        :param context: The context of the testcase.
        :param case: The testcase to get the input from.
        """
        format_ = 'text'  # By default, we use text as input.
        if case.description:
            text = case.description
        elif isinstance(case, AdditionalTestcase) and isinstance(case.function, FunctionCall):
            name = self.language_config.user_friendly_submission_name(context)
            text = self.function_call(name, case.function)
            format_ = self.config.programming_language
        elif case.stdin != NoneChannelState.NONE:
            assert isinstance(case.stdin, TextData)
            text = case.stdin.get_data_as_string(self.config.resources)
        else:
            assert isinstance(case, ExecutionTestcase)
            if case.arguments:
                variable_part = str(case.arguments)
            else:
                variable_part = "without arguments"
            text = f"Program main_testcase {variable_part}"
        return ExtendedMessage(description=text, format=format_)


CONFIGS = {
    'python': PythonConfig,
    'java': JavaConfig,
    'haskell': HaskellConfig,
    'java-scripted': JavaScriptedConfig
}


def get_runner(config: Config, language: str = None) -> BaseRunner:
    """Get the runner for the specified language."""
    if language is None:
        language = config.programming_language
    adjusted_config = replace(config, programming_language=language)
    return ConfigurableRunner(adjusted_config, CONFIGS[language]())
