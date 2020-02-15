"""
Generates code for the testplan execution.

The API entry point is the :class:`Runner` class. The main responsibility of this
class is translate and execute a testplan.

Broadly speaking, the responsibilities can be divided into a few stages:
1. Generate the code for the execution of the testplan.
2. Execute the generated code.

"""
import logging
import random
import shutil
import string
import subprocess
import tempfile
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Tuple, List, Optional, Set, Protocol

from dodona import Message, Status, ExtendedMessage
from runners.config import LanguageConfig
from runners.languages.haskell import HaskellConfig
from runners.languages.java import JavaConfig
from runners.languages.python import PythonConfig
from runners.translator import Translator, TestcaseArguments, SelectorArguments, \
    MainTestcaseArguments, ContextArguments, CustomEvaluatorArguments
from serialisation import Value, NothingType, NothingTypes, SequenceTypes, \
    SequenceType
from tested import Config
from testplan import Context, Testcase, Plan, IgnoredChannelState, \
    NoneChannelState, \
    SpecificEvaluator, NormalTestcase, FunctionInput, NoMainTestcase, \
    AssignmentInput, ExecutionMode, TestPlanError

logger = logging.getLogger(__name__)


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
    context: Context
    number: int
    mode: ExecutionMode
    common_directory: Path
    files: List[str]
    precompilation_result: Optional[Tuple[List[Message], Status]]


@dataclass
class GenerationArguments:
    """Arguments needed to generate a context file."""
    context: Context
    context_name: str
    submission_name: str


@dataclass
class CompilationResult:
    result: Optional[BaseExecutionResult]
    main: str
    dependencies: List[str]


class SupportsRunner(Protocol):
    """
    A collection of callbacks implementing the exercise evaluation pipeline. Most
    callbacks return, among other stuff, a list of files. This list will be used as
    the input for the next step in the pipeline.
    """

    def generation(self,
                   plan: Plan,
                   working_directory: Path,
                   dependencies: List[str],
                   mode: ExecutionMode) -> List[str]:
        """
        The generation step is responsible for generating the code using the
        templates for the programming language. Only generation is expected from
        this step; compilation is handled in other steps.

        :param plan: The testplan to generate files for.
        :param working_directory: The directory in which the files must be
                                  generated.
        :param dependencies: A list of files that will be made available in the
                             working directory.
        :param mode: For which execution mode files must be generated. In
                     precompilation mode, the code for a single executable for
                     all contexts must be generated as well.
        :return: A list of generated files, in the working directory, intended for
                 further processing in the pipeline.
        """
        pass

    def compilation(self,
                    working_directory: Path,
                    dependencies: List[str]
                    ) -> Tuple[Optional[BaseExecutionResult], List[str]]:
        """
        The compilation step in the pipeline. This callback is used in both the
        precompilation and individual mode. The implementation may only depend on
        the
        arguments.

        In individual compilation mode, this function may be called in a multi-
        threaded environment. Since the implementation is obvious to which mode
        it is
        operating in, it must be thread-safe.

        In individual mode, this function is responsible for compiling the code,
        such
        that a single context can be executed for evaluation. The compilation
        happens
        for each context, just before execution.

        In precompilation mode, the function is responsible for compiling all
        code at
        once. In some languages, this means the compilation will fail if one context
        is not correct. For those languages, the judge will fallback to individual
        compilation. This fallback does come with a heavy execution speed
        penalty, so
        disabling the fallback if not needed is recommended.
        :param working_directory: The directory in which the dependencies are
                                  available and in which the compilation results
                                  should be stored.
        :param dependencies: A list of files available for compilation. This list
                             will contain the results
        :return: A tuple containing an optional compilation result, and a list of
                 files, intended for further processing in the pipeline. For
                 languages without compilation, the dependencies can be returned
                 verbatim and without compilation results. Note that the judge might
                 decide to fallback to individual mode if the compilation result is
                 not positive.
        """
        pass

    def execute(self,
                executable_file: str,
                working_directory: Path,
                dependencies: List[str],
                stdin: str,
                context_argument: Optional[str] = None) -> ExecutionResult:
        """
        Execute a file.

        Note that this method must be thread-safe.

        :param dependencies: A list of files that are available in the given working
                             directory. The
        :param working_directory: The working directory, in which the execution must
                                  take place.
        :param context_argument: Argument for the executable, optional.
        :param stdin: The stdin for the execution.
        :param executable_file: The executable that should be executed. This file
                                will not be present in the dependency list.
        """
        pass

    def get_readable_input(self,
                           submission_name: str,
                           case: Testcase) -> ExtendedMessage:
        pass


class ConfigurableRunner(SupportsRunner):
    """A runner implementation based on a language config."""

    def __init__(self, config: Config, language_config: LanguageConfig,
                 translator: Translator):
        self.config = config
        self.language_config = language_config
        self.identifier = _get_identifier()
        self.translator = translator

    def generation(self,
                   plan: Plan,
                   working_directory: Path,
                   dependencies: List[str],
                   mode: ExecutionMode) -> List[str]:

        submission_name = self.language_config.submission_name(plan)
        logger.debug(f"Generating files with submission name {submission_name}")
        generated_files = dependencies.copy()
        context_names = []
        for context in plan.get_contexts():
            name = self.language_config.context_name(context)
            logger.debug(f"Generating file for context {context}")
            generated = self.generate_context(
                destination=working_directory,
                context=context,
                context_name=name,
                submission_name=submission_name
            )
            generated_files.append(generated)
            context_names.append(name)

        if mode == ExecutionMode.PRECOMPILATION:
            logger.debug("Generating selector for PRECOMPILATION mode.")
            generated = self.generate_selector(
                destination=working_directory,
                context_names=context_names
            )
            generated_files.append(generated)

        return generated_files

    def compilation(self,
                    working_directory: Path,
                    dependencies: List[str]
                    ) -> Tuple[Optional[BaseExecutionResult], List[str]]:
        command, files = self.language_config.generation_callback(dependencies)
        logger.debug("Generating files with command %s in directory %s",
                     command, working_directory)
        result = self._compile(command, working_directory)

        return result, files

    def execute(self,
                executable_file: str,
                working_directory: Path,
                dependencies: List[str],
                stdin: str,
                context_argument: Optional[str] = None) -> ExecutionResult:
        logger.info("Starting execution on file %s", executable_file)

        command = self.language_config.execution_command(
            file=executable_file,
            dependencies=dependencies,
            arguments=[context_argument] if context_argument else []
        )
        logger.debug("Executing with command %s in directory %s", command,
                     working_directory)
        # noinspection PyTypeChecker
        p = subprocess.run(command, input=stdin, text=True,
                           capture_output=True, cwd=working_directory)
        identifier = f"--{self.identifier}-- SEP"

        try:
            # noinspection PyTypeChecker
            with open(self._value_file(working_directory), "r") as f:
                values = f.read()
        except FileNotFoundError:
            logger.warning("Value file not found, looked in %s",
                           self._value_file(working_directory))
            values = ""

        try:
            # noinspection PyTypeChecker
            with open(self._exception_file(working_directory),
                      "r") as f:
                exceptions = f.read()
        except FileNotFoundError:
            logger.warning("Exception file not found, looked in %s",
                           self._exception_file(working_directory))
            exceptions = ""

        return ExecutionResult(
            stdout=p.stdout,
            stderr=p.stderr,
            exit=p.returncode,
            separator=identifier,
            results=values,
            exceptions=exceptions
        )

    def generate_selector(self,
                          destination: Path,
                          context_names: List[str]) -> str:
        """
        Generate the file to execute a single context.
        :param destination: Where the generated files should go.
        :param context_names: The names of the contexts.
        :return: The name of the generated file in the given destination.
        """

        return self.translator.write_selector_template(
            SelectorArguments(contexts=context_names),
            destination
        )

    def _value_file(self, working_directory: Path):
        return working_directory / f"{self.identifier}_values.txt"

    def _exception_file(self, working_directory: Path):
        return working_directory / f"{self.identifier}_exceptions.txt"

    def _compile(self, command: List[str], working_directory) \
            -> Optional[BaseExecutionResult]:
        if command:
            p = subprocess.run(command, text=True, capture_output=True,
                               cwd=working_directory)
            return BaseExecutionResult(p.stdout, p.stderr, p.returncode)
        else:
            return None

    def _get_custom_code(self, testcase: Testcase, function_name: str) -> str:
        has_specific = not isinstance(
            testcase.output.exception,
            (IgnoredChannelState, NoneChannelState)
        )
        if has_specific and isinstance(
                testcase.output.exception.evaluator,
                SpecificEvaluator
        ):
            custom_code = testcase.output.exception.evaluator.evaluators[
                self.config.programming_language] \
                .get_data_as_string(self.config.resources)
            return self.language_config.rename_evaluator(custom_code,
                                                         function_name)
        else:
            return self.language_config.exception_writer(function_name)

    def _get_normal_testcases(self, submission_name: str,
                              context: Context) -> List[TestcaseArguments]:
        result = []
        testcase: NormalTestcase  # Type hint for PyCharm
        for i, testcase in enumerate(context.normal):
            v_eval_function_name = f"v_evaluate_{i}"
            e_eval_function_name = f"e_evaluate_{i}"
            has_specific_v = not isinstance(
                testcase.output.result, (IgnoredChannelState, NoneChannelState)
            )
            if has_specific_v and isinstance(testcase.output.result.evaluator,
                                             SpecificEvaluator):
                custom_v_code = testcase.output.result.evaluator.evaluators[
                    self.config.programming_language
                ].get_data_as_string(self.config.resources)
                custom_v_code = self.language_config.rename_evaluator(
                    custom_v_code,
                    v_eval_function_name
                )
            else:
                custom_v_code = self.language_config.value_writer(
                    v_eval_function_name
                )
            custom_e_code = self._get_custom_code(
                testcase, e_eval_function_name
            )
            # Convert the function call.
            has_return = (testcase.output.result != NoneChannelState.NONE and
                          isinstance(testcase.input, FunctionInput))
            if has_return or isinstance(testcase.input, FunctionInput):
                statement = self.translator.prepare_function_call(
                    submission_name,
                    testcase.input.function
                )
            else:
                assert isinstance(testcase.input, AssignmentInput)
                statement = testcase.input.assignment.replace_function(
                    self.translator.prepare_function_call(
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

    def _find_paths_to_template_folder(self, files: List[str],
                                       destination: Path):
        # Copy files to the common directory.
        files_to_copy = []
        paths = self.translator.path_to_templates()
        for file in files:
            for potential_path in paths:
                if (full_file := potential_path / file).exists():
                    files_to_copy.append(full_file)
                    break
            else:  # no break
                raise ValueError(
                    f"Could not find dependency file {file}, looked in {paths}")
        for file in files_to_copy:
            # noinspection PyTypeChecker
            shutil.copy2(file, destination)

    def evaluate_custom(self,
                        path: Path,
                        expected: Optional[Value],
                        actual: Optional[Value],
                        arguments: List[Value]) -> BaseExecutionResult:

        with tempfile.TemporaryDirectory() as directory:
            # directory = "custom-dir"
            directory = Path(directory)
            logger.info("Will do custom evaluation in %s", directory)

            # Copy dependencies to the directory.
            dependencies = self.language_config.initial_dependencies() \
                + self.language_config.evaluator_dependencies()
            self._find_paths_to_template_folder(dependencies, directory)

            # Copy the custom evaluator to the execution folder.
            name = self.language_config.evaluator_name()
            destination = directory / (
                    name + "." + self.language_config.file_extension())
            source = Path(self.config.resources) / path
            logger.debug("Copying custom evaluator %s to %s", source,
                         destination)
            shutil.copy2(source, destination)

            data = CustomEvaluatorArguments(
                evaluator=name,
                expected=expected or NothingType(NothingTypes.NOTHING),
                actual=actual or NothingType(NothingTypes.NOTHING),
                arguments=SequenceType(SequenceTypes.LIST, arguments)
            )
            name = self.translator.custom_evaluator(data, directory)

            # Do compilation for those languages that require it.
            command, files = self.language_config.evaluator_generation_callback(
                dependencies + [name])
            logger.debug("Compiling custom evaluator with command %s", command)
            result = self._compile(command, directory)
            if result and result.stderr:
                raise TestPlanError(f"Error while compiling specific "
                                    f"test case: {result.stderr}")

            # Execute the custom evaluator.
            command = self.language_config.execute_evaluator(name, files)
            logger.debug("Executing custom evaluator with command %s", command)
            p = subprocess.run(command, text=True, capture_output=True,
                               cwd=directory)
            logger.debug("Custom evaluator exited with code %d", p.returncode)
            logger.debug("  Stdout was %s", p.stdout)
            logger.debug("  Stderr was %s", p.stderr)
            return BaseExecutionResult(p.stdout, p.stderr, p.returncode)

    def generate_context(self, destination: Path,
                         context: Context,
                         context_name: str,
                         submission_name: str) -> str:
        """
        Generate the files related to the context.
        :param destination: Where the generated files should go.
        :param context: The context for which generation is happening.
        :param context_name: The name of the context module.
        :param submission_name: The name of the submission from the user.
        :return: The name of the generated file in the given destination.
        """

        value_file = self._value_file(destination).name
        exception_file = self._exception_file(destination).name

        before_code = context.before.get(self.config.programming_language, "")
        after_code = context.after.get(self.config.programming_language, "")

        additional_testcases = self._get_normal_testcases(
            submission_name,
            context
        )
        # Get the main testcase.
        if context.main == NoMainTestcase.NONE:
            main_testcase = MainTestcaseArguments(
                exists=False,
                exception_code="",
                arguments=[]
            )
        else:
            eval_function_name = f"e_evaluate_main"
            custom_code = self._get_custom_code(
                context.main,
                eval_function_name
            )
            main_testcase = MainTestcaseArguments(
                exists=True,
                arguments=context.main.input.arguments,
                exception_code=custom_code
            )

        context_argument = ContextArguments(
            context_name=context_name,
            before=before_code,
            after=after_code,
            main_testcase=main_testcase,
            additional_testcases=additional_testcases,
            value_file=value_file,
            exception_file=exception_file,
            submission_name=submission_name,
            secret_id=self.identifier
        )

        return self.translator.write_context_template(
            context_argument,
            destination
        )

    def get_readable_input(self,
                           submission_name: str,
                           case: Testcase) -> ExtendedMessage:
        return self.translator.get_readable_input(submission_name, case)


CONFIGS = {
    'python':  PythonConfig,
    'java':    JavaConfig,
    'haskell': HaskellConfig
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


def get_language_config(language: str) -> LanguageConfig:
    return CONFIGS[language]()


def get_generator(config: Config, language: str = None) -> SupportsRunner:
    """Get the runner for the specified language."""
    if language is None:
        language = config.programming_language
    adjusted_config = replace(config, programming_language=language)
    language_config = get_language_config(language)
    return ConfigurableRunner(
        config=adjusted_config,
        language_config=language_config,
        translator=Translator(adjusted_config, language_config)
    )
