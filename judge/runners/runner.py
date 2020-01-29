"""
Generates code for the testplan execution.

The API entry point is the :class:`Runner` class. The main responsibility of
this class is translate and execute a testplan.

Broadly speaking, the responsibilities can be divided into a few stages:
1. Generate the code for the execution of the testplan.
2. Execute the generated code.

"""
import random
import shutil
import string
import subprocess
import tempfile
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Tuple, List, Optional, Set

from dodona import ExtendedMessage
from runners.config import LanguageConfig
from runners.dependencies import Destination, File, Dependencies
from runners.translator import ContextArguments, EvaluatorArguments, Translator, PlanContextArguments, \
    PlanEvaluatorArguments, TestcaseArguments, MainTestcaseArguments
from runners.languages.haskell import HaskellConfig
from runners.languages.java import JavaConfig
from runners.languages.jshell import JshellConfig
from runners.languages.python import PythonConfig
from runners.templates import CustomData
from serialisation import Value
from tested import Config
from testplan import Context, FunctionCall, SpecificEvaluator, IgnoredChannelState, NoneChannelState, \
    TestPlanError, NoMainTestcase, Testcase, NormalTestcase, Plan, FunctionInput, \
    AssignmentInput, FunctionType


def _get_identifier() -> str:
    """Generate a random identifier valid in most languages."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


@dataclass
class BaseExecutionResult:
    """
    Base result of executing a command.
    """
    stdout: str
    stderr: str
    exit: int


@dataclass
class ExecutionResult(BaseExecutionResult):
    """
    The result of a main testcase execution.

    All output streams are divided per testcase, in the same order as the
    context that was used to execute the test. E.g. the string at position 0 in
    stdout is the result of executing the testcase at position 0 in the context.
    """
    separator: str
    results: str
    exceptions: str


@dataclass
class ContextExecution:
    """
    Arguments used to execute a single context of the testplan.
    """
    testplan: Plan
    context: Context
    number: int
    working_directory: Path
    common_directory: Path
    files: List[str]


class Runner:
    """
    Base runner to test a submission in a given programming language.

    For most language, it is enough to define a :class:`LanguageConfig`, and use
    the pre-made :class:`ConfigurableRunner`.

    The lifecycle is basically as follows:

    - :func:`generate` is called, allowing for common things to be prepared.
    - The main method is :func:`execute`, which will execute the tests for one
      context, and report back the results.

    This class and it's implementations must be thread-safe.
    """

    def __init__(self, config: Config):
        self.config = config

    def generate(self, plan: Plan, working_directory: Path) -> Tuple[Optional[BaseExecutionResult], List[str]]:
        """
        Generate the code for a given testplan in a given directory.

        :param plan: The testplan to generate the code for.
        :param working_directory: The working directory, in which files are
               available and in which they should be generated.
        :return: An optional compilation result. If no result, it is assumed
                 generating the code went well. This can be useful when the
                 language needs compilation, for example Java and Haskell.
                 The second item in the tuple is list of generated files that
                 should be made available to the context execution.
        """
        raise NotImplementedError

    def execute(self, arguments: ContextExecution) -> ExecutionResult:
        """
        Execute the tests for a given context. This function typically implements following steps:

        1. Generate the code necessary for executing the tests for this context.
        2. (Optional) Compile the code.
        3. Run the code and report back the result.

        Note that this method should be thread-safe: multiple context may be evaluated in parallel.

        :param arguments: The arguments for this execution.
        """
        raise NotImplementedError

    def evaluate_custom(self, code: str, expected: Value, actual: Value) -> BaseExecutionResult:
        """Evaluate two values with code provided by the test plan."""
        raise NotImplementedError

    def get_readable_input(self, case: Testcase) -> ExtendedMessage:
        raise NotImplementedError


class ConfigurableRunner(Runner):
    """A runner implementation based on a language config."""

    def __init__(self, config: Config, language_config: LanguageConfig, translator: Translator):
        super().__init__(config)
        self.language_config = language_config
        self.identifier = _get_identifier()
        self.translator = translator
        self.files = Dependencies(
            [File(x, Destination.PRE_COMPILATION) for x in language_config.initial_dependencies()]
        )

    def _write_contexts_template(self,
                                 plan: Plan,
                                 contexts: List[Context],
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
        context_arguments = []
        evaluator_arguments = []
        value_file = str(self._value_file(destination)).replace("\\", "/")
        exception_file = str(self._exception_file(destination)).replace("\\", "/")
        for i, context in enumerate(contexts):
            # The code for before and after the context.
            before_code = context.before.get(self.config.programming_language, "")
            after_code = context.after.get(self.config.programming_language, "")

            additional_testcases = self._get_additional_testcases(plan, context, i)
            main_testcase = self._get_main_testcase(context, i)

            context_arguments.append(ContextArguments(
                before=before_code,
                after=after_code,
                main_testcase=main_testcase,
                additional_testcases=additional_testcases,
            ))
            evaluator_arguments.append(EvaluatorArguments(
                main_testcase=main_testcase,
                additional_testcases=additional_testcases
            ))

        context_file = self.translator.write_plan_context_template(
            PlanContextArguments(
                secret_id=self.identifier,
                contexts=context_arguments,
                value_file=value_file,
                exception_file=exception_file,
                submission_name=submission_name
            ),
            destination
        )

        evaluator_file = self.translator.write_plan_evaluator_template(
            PlanEvaluatorArguments(
                value_file=value_file,
                exception_file=exception_file,
                secret_id=self.identifier,
                evaluators=evaluator_arguments
            ),
            destination
        )

        return [evaluator_file, context_file]

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

    def generate(self, plan: Plan, working_directory: Path) -> Tuple[Optional[BaseExecutionResult], List[str]]:
        # Write the submission file to the correct location.
        submission_name = self.language_config.submission_name(plan)
        submission = submission_name + "." + self.language_config.file_extension()
        # noinspection PyTypeChecker
        shutil.copy2(self.config.source, working_directory / submission)

        dependencies = [submission]

        # Copy files to the common directory.
        files_to_copy = []
        paths = self.translator.path_to_templates()
        for file in self.language_config.initial_dependencies():
            for potential_path in paths:
                if (full_file := potential_path / file).exists():
                    files_to_copy.append(full_file)
                    dependencies.append(file)
                    break
            else:  # no break
                raise ValueError(f"Could not find dependency file {file}, looked in {paths}")
        for file in files_to_copy:
            # noinspection PyTypeChecker
            shutil.copy2(file, working_directory)

        # Generate all relevant files.
        c = [c for tab in plan.tabs for c in tab.contexts]
        generated_files = self._write_contexts_template(plan, c, submission_name, working_directory)
        dependencies.extend(generated_files)

        command, files = self.language_config.generation_callback(dependencies)
        print(f"Generation command is {command}, files are {files}, dependencies are {dependencies}")
        result = self._compile(command, working_directory)

        return result, files

    def execute(self, arguments: ContextExecution) -> ExecutionResult:

        # Copy the files we need to our own directory, to make them independent.
        for file in arguments.files:
            print(f"Copying {arguments.common_directory / file} to {arguments.working_directory}")
            # noinspection PyTypeChecker
            shutil.copy2(arguments.common_directory / file, arguments.working_directory)

        # Actually execute the testcode.
        stdin_ = []
        for testcase in arguments.context.all_testcases():
            if (input_ := testcase.input.get_as_string(self.config.resources)) is not None:
                stdin_.append(input_)
        stdin_ = "\n".join(stdin_)

        command = self.language_config.execution_command(arguments.files)
        print(f"Executing with command {command}\n")
        # noinspection PyTypeChecker
        p = subprocess.run(command, input=stdin_, text=True, capture_output=True, cwd=arguments.working_directory)
        identifier = f"--{self.identifier}-- SEP"

        try:
            # noinspection PyTypeChecker
            with open(self._value_file(arguments.working_directory), "r") as f:
                values = f.read()
        except FileNotFoundError:
            values = ""

        try:
            # noinspection PyTypeChecker
            with open(self._exception_file(arguments.working_directory), "r") as f:
                exceptions = f.read()
        except FileNotFoundError:
            exceptions = ""

        return ExecutionResult(p.stdout, p.stderr, p.returncode, identifier, values, exceptions)

    def _get_additional_testcases(self, plan: Plan, context: Context, number: int) -> List[TestcaseArguments]:
        result = []
        testcase: NormalTestcase  # Type hint for PyCharm
        for i, testcase in enumerate(context.normal):
            v_eval_function_name = f"v_evaluate_{number}_{i}"
            e_eval_function_name = f"e_evaluate_{number}_{i}"
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
            has_return = testcase.output.result != NoneChannelState.NONE and isinstance(testcase.input, FunctionInput)
            if has_return or isinstance(testcase.input, FunctionInput):
                statement = self.prepare_function_call(submission_name, testcase.input.function)
            else:
                assert isinstance(testcase.input, AssignmentInput)
                statement = testcase.input.assignment.replace_function(self.prepare_function_call(
                    submission_name,
                    testcase.input.assignment.expression
                ))

            result.append(TestcaseArguments(
                statement=statement,
                stdin=testcase.input.stdin,
                value_code=custom_v_code,
                exception_code=custom_e_code,
                has_return=has_return
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

    def _get_main_testcase(self, context: Context, number: int) -> MainTestcaseArguments:
        if context.main == NoMainTestcase.NONE:
            return MainTestcaseArguments(exists=False, exception_code="", arguments=[])
        else:
            eval_function_name = f"e_evaluate_main_{number}"
            custom_code = self._get_custom_code(context.main, eval_function_name)
            return MainTestcaseArguments(exists=True, arguments=context.main.input.arguments, exception_code=custom_code)

    def prepare_function_call(self, submission_name: str, function_call: FunctionCall) -> FunctionCall:
        """Prepare the function call for main."""
        if function_call.type == FunctionType.IDENTITY:
            return function_call
        object_ = function_call.object or submission_name
        return FunctionCall(
            type=function_call.type,
            arguments=function_call.arguments,
            name=self.language_config.conventionalise(function_call.name),
            object=object_
        )

    def evaluate_custom(self, code: str, expected: Value, actual: Value) -> BaseExecutionResult:
        # Create the template.
        template = self._find_template("evaluator_custom", self._get_environment)
        data = CustomData(evaluator_code=code, expected=expected, actual=actual)
        # directory = Path(self.config.workdir, f"specific")
        # directory.mkdir()
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

    def get_readable_input(self, case: Testcase) -> ExtendedMessage:
        return self.translator.get_readable_input(case)


CONFIGS = {
    'python':  PythonConfig,
    'java':    JavaConfig,
    'haskell': HaskellConfig,
    'jshell':  JshellConfig
}


def get_languages() -> Set[str]:
    """
    :return: Languages supported by the judge.
    """
    return set(CONFIGS.keys())


def get_supporting_languages(plan: Plan) -> Set[str]:
    """
    :param plan: The testplan.
    :return: The languages that have the required features to execute the testplan.
    """
    required = plan.get_used_features()
    supported_languages = set()
    for language, config in CONFIGS.items():
        supported_features = config().supported_features()
        if supported_features & required != 0:
            supported_languages.add(language)
    return supported_languages


def get_generator(config: Config, language: str = None) -> Runner:
    """Get the runner for the specified language."""
    if language is None:
        language = config.programming_language
    adjusted_config = replace(config, programming_language=language)
    language_config = CONFIGS[language]()
    return ConfigurableRunner(adjusted_config, language_config, Translator(config, language_config))
