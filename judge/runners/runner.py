"""Code generators for the testplans."""
import random
import shutil
import string
import subprocess
import tempfile
from dataclasses import dataclass, replace
from functools import cached_property
from pathlib import Path
from typing import Tuple, List

from mako.exceptions import TemplateLookupException
from mako.lookup import TemplateLookup

from runners.config import LanguageConfig
from runners.haskell import HaskellConfig
from runners.java import JavaConfig
from runners.javascripted import JavaScriptedConfig
from runners.python import PythonConfig
from runners.templates import SubmissionData, write_template, ContextData, TestcaseData, EvaluatorData, CustomData
from runners.utils import remove_indents, remove_newline
from serialisation import Value
from tested import Config
from testplan import Plan, Context, FunctionCall, SpecificEvaluator, IgnoredChannelState


def _get_identifier() -> str:
    """Generate a random identifier valid in most languages."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


@dataclass
class BaseExecutionResult:
    """
    The result of an execution.
    """
    stdout: str
    stderr: str
    exit: int  # The return value.


@dataclass
class ExecutionResult(BaseExecutionResult):
    """
    The result of an execution.

    All output streams are divided per testcase, in the same order as the context that was used to
    execute the test. E.g. the string at position 0 in stdout is the result of executing the
    testcase at position 0 in the context.
    """
    separator: str
    results: str


class BaseRunner:
    """
    Base runner to test a submission in a given programming language.

    For most language, it is enough to define a :class:`LanguageConfig`, and use the pre-made
    :class:`ConfigurableRunner`.
    """

    def __init__(self, config: Config):
        self.config = config

    def generate_code(self, submission: str, plan: Plan) -> Tuple[List[str], List[str]]:
        """
        Generate the code necessary for execution. All code should be generated for the whole
        testplan.

        Each context in the test plan is given an unique ID, which can be used to retrieve the
        results of the execution later.

        :param submission: The code that must be tested.
        :param plan: The test plan to execute for the given submission.
        :return: A list of context ids and an ordered list of files.
        """
        raise NotImplementedError

    def execute(self, context_id: str, context: Context, timeout=None) -> ExecutionResult:
        """
        Execute the tests for a given context.

        :param timeout: Time-out for the evaluation of the context.
        :param context_id: The ID of the context. This is an element returned to the list by the
        generation function.
        :param context: The actual context instance.
        :return: The result of the execution, which allows judging of the results.
        """
        raise NotImplementedError

    def compile(self, ordered_files) -> BaseExecutionResult:
        """Will be called if `needs_compilation` is True."""
        raise NotImplementedError

    def needs_compilation(self) -> bool:
        """True if the language needs an implementation step."""
        raise NotImplementedError

    def function_call(self, call: FunctionCall) -> str:
        """Create a textual representation of a function call, to display to the user."""
        raise NotImplementedError

    def evaluate_specific(self, code: str, expected: Value, actual: Value) -> BaseExecutionResult:
        """Evaluate two values with code provided by the test plan."""
        raise NotImplementedError


class ConfigurableRunner(BaseRunner):
    """Base runner working with language configurations."""

    def __init__(self, config: Config, language_config: LanguageConfig):
        super().__init__(config)
        self.language_config = language_config
        self.identifier = _get_identifier()

    def _write_submission_template(self,
                                   submission: str,
                                   submission_name: str,
                                   before: str,
                                   after: str) -> List[str]:
        """
        Write the submission template to file.

        The parameters of this function are available to the template. Note that just because they
        are available, does not mean they need to be used. For example, the before and after should
        be executed where it makes sense for the language. For example, in Python, this is in the
        module of the user's code, but in Java, this is in the main of the context.

        :param submission: The submitted code.
        :param submission_name: The name of the container for the submitted code. For example, in
                                Java this will be the class, in Haskell the module.
        :param before: The "before" code for the user's code.
        :param after: The "after" code for the user's code.
        :return: The files that were generated, in order of dependencies.
        """
        submission_template = self._find_template("submission", self._get_environment)
        data = SubmissionData(submission=submission, submission_name=submission_name, before=before, after=after)
        submission_file = f"{submission_name}.{self.language_config.file_extension()}"
        write_template(data, submission_template, Path(self.config.workdir, submission_file))
        return [submission_file]

    def _write_context_template(self,
                                context: Context,
                                submission: str,
                                context_id: str,
                                submission_name: str,
                                before: str,
                                after: str) -> List[str]:
        """
        Write the context template to file.

        The parameters of this function are available to the template. Note that just because they
        are available, does not mean they need to be used. For example, the before and after should
        be executed where it makes sense for the language. For example, in Python, this is in the
        module of the user's code, but in Java, this is in the main of the context.

        :param context: The context to execute.
        :param submission: The submitted code.
        :param context_id: The id of the context.
        :param submission_name: The name of the container for the submitted code. For example, in
                                Java this will be the class, in Haskell the module.
        :param before: The "before" code for the user's code.
        :param after: The "after" code for the user's code.
        :return: The files that were generated, in order of dependencies.
        """
        context_template = self._find_template("context", self._get_environment)
        return_file = str(self._result_file()).replace("\\", "/")
        # Create the test file.
        additionals = self.get_additional(context_id, context)
        data = ContextData(
            main_arguments=context.execution.arguments,
            submission=submission,
            secret_id=self.identifier,
            output_file=return_file,
            additionals=additionals,
            context_id=context_id,
            has_top_level=self.language_config.supports_top_level_functions(),
            submission_name=submission_name,
            before=before,
            after=after,
        )
        write_template(data, context_template, Path(self.config.workdir, self._context_name(context_id)))

        evaluator_template = self._find_template("evaluators", self._get_environment)
        evaluator_data = EvaluatorData(
            additionals=additionals,
            output_file=return_file,
            context_id=context_id,
        )
        write_template(evaluator_data, evaluator_template,
                       Path(self.config.workdir, self._evaluator_name(context_id)))

        return [self._evaluator_name(context_id), self._context_name(context_id)]

    def generate_code(self, submission: str, plan: Plan) -> Tuple[List[str], List[str]]:

        ordered_files = []

        # Copy additional files we might need.
        for file in self.language_config.additional_files():
            result = self._path_to_templates() / file
            # noinspection PyTypeChecker
            shutil.copy2(result, self.config.workdir)
        ordered_files.extend(self.language_config.additional_files())

        # Each context gets two files: the file containing the user code, and the file containing
        # the actual tests we run on the user code.
        # Language that have the same user code for all contexts can return the same name each
        # time, which causes the files to be overridden. This will be faster if compilation is needed.
        # TODO: In the future, we might not generate different files if the name is the same.
        context_ids = []
        for tab_idx, tab in enumerate(plan.tabs):
            for context_idx, context in enumerate(tab.contexts):
                # The ID for this context.
                id_ = f"{tab_idx}_{context_idx}"
                # The before and after.
                before = context.before.get(self.config.programming_language, "")
                after = context.after.get(self.config.programming_language, "")
                submission_name = self.language_config.submission_name(id_, context)

                # Create the submission file if necessary.
                if submission_name:
                    submission_files = self._write_submission_template(submission, submission_name, before, after)
                    ordered_files.extend(submission_files)

                context_files = self._write_context_template(context, submission, id_, submission_name, before, after)
                ordered_files.extend(context_files)
                context_ids.append(id_)

        return context_ids, ordered_files

    def _submission_name(self, name):
        return f"{name}.{self.language_config.file_extension()}"

    def _result_file(self):
        return Path(self.config.workdir, f"{self.identifier}_out.txt")

    def compile(self, ordered_files, cwd=None) -> BaseExecutionResult:
        cwd = cwd or self.config.workdir
        command = self.language_config.compilation_command(ordered_files)
        p = subprocess.run(command, text=True, capture_output=True, cwd=cwd)
        return BaseExecutionResult(p.stdout, p.stderr, p.returncode)

    def _context_name(self, context_id: str) -> str:
        return f"{self.language_config.context_name(context_id)}.{self.language_config.file_extension()}"

    def _evaluator_name(self, context_id: str) -> str:
        return f"{self.language_config.evaluator_name(context_id)}.{self.language_config.file_extension()}"

    def needs_compilation(self) -> bool:
        return self.language_config.needs_compilation()

    def execute(self, context_id: str, context: Context, timeout=None) -> ExecutionResult:

        stdin_ = []
        for testcase in context.all_testcases():
            if (input_ := testcase.get_input(self.config.resources)) is not None:
                stdin_.append(input_)
        stdin_ = "\n".join(stdin_)

        command = self.language_config.execution_command(context_id)
        p = subprocess.run(command, input=stdin_, timeout=timeout, text=True, capture_output=True,
                           cwd=self.config.workdir)
        identifier = f"--{self.identifier}-- SEP"

        try:
            with open(self._result_file(), "r") as f:
                values = f.read()
        except FileNotFoundError:
            values = ""

        return ExecutionResult(p.stdout, p.stderr, p.returncode, identifier, values)

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

    def function_call(self, call: FunctionCall) -> str:
        template = self._find_template("function", self._get_environment)
        return template.render(function=call)

    def get_additional(self, context_id: str, context: Context) -> List[TestcaseData]:
        result = []
        for i, testcase in enumerate(context.additional):
            eval_function_name = f"evaluate_{context_id}_{i}"
            has_specific = not isinstance(testcase.result, IgnoredChannelState)
            if has_specific and isinstance(testcase.result.evaluator, SpecificEvaluator):
                custom_code = testcase.result.evaluator.evaluators[self.config.programming_language] \
                    .get_data_as_string(self.config.resources)
                custom_code = self.language_config.rename_evaluator(custom_code, eval_function_name)
            else:
                # Get value function call.
                custom_code = self.language_config.value_writer(eval_function_name)
            # Convert the function call.
            submission_name = self.language_config.submission_name(context_id, context)
            result.append(TestcaseData(
                function=self.prepare_function_call(submission_name, testcase.function),
                stdin=testcase.stdin,
                value_code=custom_code
            ))
        return result

    def prepare_function_call(self, submission_name: str, function_call: FunctionCall) -> FunctionCall:
        """Prepare the function call for execution."""
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
            path = Path(directory) / self._evaluator_name("eval")
            write_template(data, template, path)

            files = self.language_config.additional_files()
            # Copy necessary files to the temporary directory.
            for file in self.language_config.additional_files():
                result = self._path_to_templates() / file
                # noinspection PyTypeChecker
                shutil.copy2(result, directory)

            files.append(self._evaluator_name("eval"))

            # Compile the evaluator
            # TODO: handle errors!
            c = self.compile(files, directory)
            # Execute the evaluator
            command = self.language_config.execute_evaluator("eval")
            p = subprocess.run(command, text=True, capture_output=True, cwd=directory)
            return BaseExecutionResult(p.stdout, p.stderr, p.returncode)


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
