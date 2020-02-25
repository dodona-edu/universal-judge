import logging
import random
import shutil
import string
import subprocess
import humps
from dataclasses import replace
from typing import Tuple

import utils
from dodona import *
from evaluators import get_evaluator, Evaluator
from runners.generator import (Generator, path_to_templates, value_file,
                               exception_file)
from runners.languages.haskell import HaskellConfig
from runners.languages.java import JavaConfig
from runners.languages.python import PythonConfig
from runners.config import LanguageConfig
from tested import Config
from testplan import *

logger = logging.getLogger(__name__)


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
    context that was used to execute_module the test. E.g. the string at position
    0 in
    stdout is the result of executing the testcase at position 0 in the context.
    """
    separator: str
    results: str
    exceptions: str


@dataclass
class ContextExecution:
    """
    Arguments used to execute_module a single context of the testplan.
    """
    context: Context
    context_name: str
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


def _get_identifier() -> str:
    """Generate a random identifier valid in most languages."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


def _evaluate_channel(
        out: IO,
        channel_name: str,
        expected_output: Union[OutputChannel, AnyChannelState],
        actual_result: Optional[str],
        evaluator: Evaluator) -> bool:
    """
    Evaluate the output on a given channel. This function will output the
    appropriate messages
    to start and end a new test in Dodona.

    If errors is given, the test will end with a runtime error. Note that if the
    expected output
    is None, the test will not be written to Dodona if everything is correct.

    :param out: The output file for the judge.
    :param channel_name: The name of the channel being evaluated. Will be
    displayed in Dodona.
    :param expected_output: The output channel from the test case.
    :param actual_result: The actual output. Can be None, depending on the
    evaluator.
    :param evaluator: The evaluator to use.
    :return: True if successful, otherwise False.
    """
    evaluation_result = evaluator.evaluate(expected_output, actual_result)
    status = evaluation_result.result

    # If the actual value is empty and the expected output is None or ignored,
    # don't report it.
    is_correct = status.enum == Status.CORRECT
    has_no_result = actual_result is None or actual_result == ""
    has_no_expected = (expected_output == NoneChannelState.NONE
                       or expected_output == IgnoredChannelState.IGNORED)
    if is_correct and has_no_result and has_no_expected:
        return True

    report_update(out, StartTest(
        expected=evaluation_result.readable_expected,
        channel=channel_name
    ))

    # Report any messages we received.
    for message in evaluation_result.messages:
        report_update(out, AppendMessage(message=message))

    # Close the test.
    report_update(out, CloseTest(
        generated=evaluation_result.readable_actual,
        status=status
    ))

    return is_correct


def _process_compile_results(
        results: Optional[BaseExecutionResult]
) -> Tuple[List[Message], Status]:
    """
    Process the output of a compilation step. It will convert the result of the
    command into a list of messages and a status. If the status is not correct,
    the messages and status may be passed to Dodona unchanged. Alternatively, they
    can be kept to show them with the first context.
    """
    messages = []

    # There was no compilation
    if results is None:
        return messages, Status.CORRECT

    show_stdout = False

    # Report stderr.
    if results.stderr:
        # Append compiler messages to the output.
        messages.append("De compiler produceerde volgende uitvoer op stderr:")
        messages.append(ExtendedMessage(
            description=results.stderr,
            format='code'
        ))
        logger.debug("Received stderr from compiler: " + results.stderr)
        show_stdout = True

    # Report stdout.
    if results.stdout and (show_stdout or results.exit != 0):
        # Append compiler messages to the output.
        messages.append("De compiler produceerde volgende uitvoer op stdout:")
        messages.append(ExtendedMessage(
            description=results.stdout,
            format='code'
        ))
        logger.debug("Received stdout from compiler: " + results.stderr)

    # Report errors if needed.
    if results.exit != 0:
        messages.append(f"Het compilatieproces eindigde met "
                        f"exitcode {results.exit}")
        return messages, Status.COMPILATION_ERROR
    else:
        return messages, Status.CORRECT


@dataclass
class MetaContext:
    """Contains a context and some metadata about the context."""
    tab_index: int  # The index of the tab this context belongs to.
    context_index: int  # The index of this context in its tab.
    context: Context  # The actual context.


CONFIGS = {
    'python':  PythonConfig,
    'java':    JavaConfig,
    'haskell': HaskellConfig
}


class GeneratorJudge:
    """
    Responsible for orchestrating the judgment of a solution. It's responsibilities
    are copying generated files between folders, running the executions and
    interpreting the results.

    Other classes of interest are:

    - :class:`Runner` for actually generating, compiling and executing code
    - :class:`Evaluator` and implementations for comparing obtained and expected
       results.
    """

    def __init__(self, config: Config, output: IO, language: Optional[str] = None):
        self.out = output
        if language is None:
            language = config.programming_language
        adjusted_config = replace(config, programming_language=language)
        language_config = CONFIGS[language]()
        self.config = adjusted_config
        self.language_config: LanguageConfig = language_config
        self.identifier = _get_identifier()
        self.translator = Generator(adjusted_config, language_config)

    def judge(self, plan: Plan):
        """
        Evaluate a solution for an exercise. Execute the tests present in the
        testplan. The result (the judgment) is sent to stdout, so Dodona can pick it
        up.
        :param plan: The plan to execute_module.
        """
        # Begin by checking if the given testplan is executable in this language.
        logger.info("Checking supported features...")
        if not self.language_config.supports(plan):
            report_update(self.out, StartJudgment())
            report_update(self.out, CloseJudgment(
                accepted=False,
                status=StatusMessage(
                    enum=Status.INTERNAL_ERROR,
                    human=f"Deze oefening kan niet opgelost worden in deze "
                          f"programmeertaal: {self.config.programming_language}"
                )
            ))
            logger.info("Required features not supported.")
            return  # Not all required features are supported.

        mode = plan.configuration.mode
        report_update(self.out, StartJudgment())

        logger.info("Start generating code...")
        common_dir, files, selector = self.generate_files(plan, mode)

        # Add the selector to the dependencies.
        if selector:
            files.append(selector)

        if mode == ExecutionMode.PRECOMPILATION:
            assert not self.language_config.needs_selector() or selector is not None
            # Compile all code in one go.
            logger.info("Running precompilation step...")
            result, compilation_files = self.compilation(common_dir, files)

            messages, status = _process_compile_results(result)
            precompilation_result = (messages, status)

            # If we have fallback, discard all results.
            if status != Status.CORRECT and plan.configuration.allow_fallback:
                mode = ExecutionMode.INDIVIDUAL
                logger.info("Compilation error, falling back to individual mode")
                # Remove the selector file from the dependencies.
                # Otherwise, it will keep being compiled, which we want to avoid.
                if self.language_config.needs_selector():
                    files.remove(selector)
            else:
                files = compilation_files
                # Report messages.
                for message in messages:
                    report_update(self.out, AppendMessage(message=message))

                if status != Status.CORRECT:
                    report_update(self.out, CloseJudgment(
                        accepted=False,
                        status=StatusMessage(
                            enum=status,
                            human="Fout tijdens compilatie van de code."
                        )
                    ))
                    logger.info("Compilation error without fallback")
                    return  # Compilation error occurred, useless to continue.
        else:
            precompilation_result = None

        logger.info("Starting judgement...")
        # pool = Pool(4 if plan.configuration.parallel else 1)

        with utils.protected_directory(common_dir) as common_dir:

            for tab_index, tab in enumerate(plan.tabs):
                report_update(self.out, StartTab(title=tab.name))
                # Create a list of arguments to execute_module (in threads)
                executions = []
                for context_index, context in enumerate(tab.contexts):
                    executions.append(ContextExecution(
                        context=context,
                        context_name=self.language_config.context_name(
                            tab_number=tab_index,
                            context_number=context_index
                        ),
                        mode=mode,
                        common_directory=common_dir,
                        files=files,
                        precompilation_result=precompilation_result
                    ))

                results = []
                for execution in executions:
                    results.append(self.execute_context(execution))
                # Do the executions in parallel
                # results = pool.map(self.execute_context, executions)

                # Handle the results
                for context_index, context in enumerate(tab.contexts):
                    report_update(self.out, StartContext(
                        description=context.description
                    ))
                    execution_result, m, s = results[context_index]
                    self.evaluate_results(
                        plan=plan,
                        context=context,
                        results=execution_result,
                        compiler_results=(m, s)
                    )
                    report_update(self.out, CloseContext())
                report_update(self.out, CloseTab())
            report_update(self.out, CloseJudgment())

    def evaluate_results(self,
                         plan: Plan,
                         context: Context,
                         results: ExecutionResult,
                         compiler_results: Tuple[List[Message], Status]):
        # Process output
        stdout_ = results.stdout.split(results.separator) \
            if results is not None else []
        stderr_ = results.stderr.split(results.separator) \
            if results is not None else []
        values = results.results.split(results.separator) \
            if results is not None else []
        exceptions = results.exceptions.split(results.separator) \
            if results is not None else []

        # There might be less output than testcase, which is an error. However,
        # we process the
        # output we have, to ensure we send as much feedback as possible to the
        # user.
        testcase: Testcase  # Type hint for pycharm.
        for i, testcase in enumerate(context.all_testcases()):

            name = self.language_config.submission_name(plan)
            readable_input = self.translator.get_readable_input(name, testcase)
            report_update(self.out, StartTestcase(description=readable_input))

            # Handle compiler results
            if compiler_results[1] != Status.CORRECT:
                for message in compiler_results[0]:
                    report_update(self.out, AppendMessage(message=message))
                report_update(self.out, EscalateStatus(
                    status=StatusMessage(enum=compiler_results[1])))
                report_update(self.out, CloseTestcase(accepted=False))
                break
            else:
                assert results is not None

            # Get the evaluators
            try:
                stdout_evaluator = get_evaluator(self, testcase.output.stdout)
                stderr_evaluator = get_evaluator(self, testcase.output.stderr)
                file_evaluator = get_evaluator(self, testcase.output.file)
                value_evaluator = get_evaluator(self, testcase.output.result)
                exception_evaluator = get_evaluator(self, testcase.output.exception)
            except TestPlanError as e:
                report_update(self.out, AppendMessage(message=ExtendedMessage(
                    description=str(e),
                    format='text',
                    permission=Permission.STAFF
                )))
                break

            # Evaluate the file channel.
            results = [_evaluate_channel(
                self.out, "file", testcase.output.file, None, file_evaluator
            )]

            # Evaluate the stderr channel
            actual_stderr = stderr_[i] if i < len(stderr_) else None
            results.append(_evaluate_channel(
                self.out, "stderr", testcase.output.stderr, actual_stderr,
                stderr_evaluator
            ))

            actual_exception = exceptions[i] if i < len(exceptions) else None
            results.append(_evaluate_channel(
                self.out, "exception", testcase.output.exception, actual_exception,
                exception_evaluator
            ))

            actual_stdout = stdout_[i] if i < len(stdout_) else None
            results.append(_evaluate_channel(
                self.out, "stdout", testcase.output.stdout, actual_stdout,
                stdout_evaluator
            ))

            actual_value = values[i] if i < len(values) else None
            results.append(
                _evaluate_channel(self.out, "return", testcase.output.result,
                                  actual_value, value_evaluator)
            )

            # Check if there is early termination.
            if i >= len(stdout_) \
                    or i >= len(stderr_) \
                    or i >= len(values) \
                    or i >= len(exceptions):
                report_update(self.out, AppendMessage(message=ExtendedMessage(
                    description="Tests were terminated early.", format='text'
                )))

            report_update(self.out, CloseTestcase())

            # If this was an essential testcase with an error, stop testing now.
            if testcase.essential and not all(results):
                break

    def generate_files(self,
                       plan: Plan,
                       mode: ExecutionMode
                       ) -> Tuple[Path, List[str], Optional[str]]:
        """
        Generate all necessary files, using the templates. This creates a common
        directory, copies all dependencies to that folder and runs the generation.
        """
        dependencies = self.language_config.initial_dependencies()
        common_dir = Path(self.config.workdir, f"common")
        common_dir.mkdir()

        logger.debug(f"Generating files in common directory %s", common_dir)

        # Copy dependencies
        dependency_paths = path_to_templates(
            self.language_config,
            self.config
        )
        utils.copy_from_paths_to_path(dependency_paths, dependencies, common_dir)

        submission_name = self.language_config.submission_name(plan)

        # Copy the submission file.
        submission_file = f"{submission_name}" \
                          f".{self.language_config.file_extension()}"
        submission_path = common_dir / submission_file
        shutil.copy2(self.config.source, submission_path)
        dependencies.append(submission_file)

        # Allow modifications of the submission file.
        self.language_config.solution_callback(submission_path, plan)

        # The names of the contexts in the testplan.
        context_names = []
        # Generate the files for each context.
        for tab_i, tab in enumerate(plan.tabs):
            for context_i, context in enumerate(tab.contexts):
                context_name = self.language_config.context_name(tab_i, context_i)
                logger.debug(f"Generating file for context {context_name}")
                generated, evaluators = self.translator.generate_context(
                    destination=common_dir,
                    context=context,
                    context_name=context_name,
                    submission_name=submission_name,
                    secret=self.identifier
                )
                # Copy evaluators to the directory.
                for evaluator in evaluators:
                    source = Path(self.config.resources) / evaluator
                    logger.debug("Copying evaluator from %s to %s",
                                 source, common_dir)
                    shutil.copy2(source, common_dir)
                dependencies.extend(evaluators)
                dependencies.append(generated)
                context_names.append(context_name)

        if mode == ExecutionMode.PRECOMPILATION \
                and self.language_config.needs_selector():
            logger.debug("Generating selector for PRECOMPILATION mode.")
            generated = self.translator.generate_selector(
                destination=common_dir,
                context_names=context_names
            )
        else:
            generated = None
        return common_dir, dependencies, generated

    def execute_context(self, args: ContextExecution) \
            -> Tuple[Optional[ExecutionResult], List[Message], Status]:
        """
        Execute a context.
        """
        # Create a working directory for the context.
        context_dir = Path(
            self.config.workdir,
            args.context_name
        )
        context_dir.mkdir()

        logger.info("Executing context %s in path %s",
                    args.context_name, context_dir)

        dependencies = self.language_config.context_dependencies_callback(
            args.context_name,
            args.files
        )

        # Copy files from the common directory to the context directory.
        for file in dependencies:
            origin = args.common_directory / file
            logger.debug("Copying %s to %s", origin, context_dir)
            # noinspection PyTypeChecker
            shutil.copy2(origin, context_dir)

        # If needed, do a compilation.
        if args.mode == ExecutionMode.INDIVIDUAL:
            logger.info("Compiling context %d in INDIVIDUAL mode...",
                        args.context_number)
            result, files = self.compilation(
                working_directory=context_dir,
                dependencies=dependencies
            )

            # Process compilation results.
            messages, status = _process_compile_results(result)

            if status != Status.CORRECT:
                logger.debug("Compilation of individual context failed.")
                logger.debug("Aborting executing of this context.")
                return None, messages, status

            logger.debug("Executing context %s in INDIVIDUAL mode...",
                         args.context_name)
            executable = self.find_main_file(files, args.context_name)
            files.remove(executable)
            stdin = args.context.get_stdin(self.config.resources)

            base_result = self.execute_file(
                executable_name=executable,
                working_directory=context_dir,
                dependencies=files,
                stdin=stdin
            )
        else:
            result, files = None, dependencies
            if args.precompilation_result:
                logger.debug("Substituting precompilation results.")
                messages, status = args.precompilation_result
            else:
                logger.debug("No precompilation results found, using default.")
                messages, status = [], Status.CORRECT

            logger.info("Executing context %s in PRECOMPILATION mode...",
                        args.context_name)

            if self.language_config.needs_selector():
                logger.debug("Selector is needed, using it.")

                selector_name = self.language_config.selector_name()
                executable = self.find_main_file(files, selector_name)
                files.remove(executable)
                stdin = args.context.get_stdin(self.config.resources)

                base_result = self.execute_file(
                    executable_name=executable,
                    working_directory=context_dir,
                    dependencies=files,
                    stdin=stdin,
                    argument=args.context_name
                )
            else:
                logger.debug("Selector is not needed, using individual execution.")
                executable = self.find_main_file(files, args.context_name)
                files.remove(executable)
                stdin = args.context.get_stdin(self.config.resources)

                base_result = self.execute_file(
                    executable_name=executable,
                    working_directory=context_dir,
                    dependencies=files,
                    stdin=stdin
                )

        identifier = f"--{self.identifier}-- SEP"

        value_file_path = value_file(context_dir, self.identifier)
        try:
            # noinspection PyTypeChecker
            with open(value_file_path, "r") as f:
                values = f.read()
        except FileNotFoundError:
            logger.warning("Value file not found, looked in %s", value_file_path)
            values = ""

        exception_file_path = exception_file(context_dir, self.identifier)
        try:
            # noinspection PyTypeChecker
            with open(exception_file_path, "r") as f:
                exceptions = f.read()
        except FileNotFoundError:
            logger.warning("Exception file not found, looked in %s",
                           exception_file_path)
            exceptions = ""

        result = ExecutionResult(
            stdout=base_result.stdout,
            stderr=base_result.stderr,
            exit=base_result.exit,
            separator=identifier,
            results=values,
            exceptions=exceptions
        )

        return result, messages, status

    def find_main_file(self, files: List[str], name: str) -> str:
        logger.debug("Finding %s in %s", name, files)
        return [x for x in files if x.startswith(name)][0]

    def run_compilation_command(
            self,
            command: List[str],
            working_directory: Path
    ) -> Optional[BaseExecutionResult]:
        if command:
            # noinspection PyTypeChecker
            p = subprocess.run(command, text=True, capture_output=True,
                               cwd=working_directory)
            return BaseExecutionResult(p.stdout, p.stderr, p.returncode)
        else:
            return None

    def execute_file(
            self,
            executable_name: str,
            working_directory: Path,
            dependencies: List[str],
            stdin: Optional[str] = None,
            argument: Optional[str] = None
    ) -> BaseExecutionResult:
        """
        Execute a file.

        Note that this method must be thread-safe.

        :param dependencies: A list of files that are available in the given working
                             directory. The
        :param working_directory: The working directory, in which the execution must
                                  take place.
        :param argument: Argument for the executable, optional.
        :param stdin: The stdin for the execution.
        :param executable_name: The executable that should be executed. This file
                                will not be present in the dependency list.
        """
        logger.info("Starting execution on file %s", executable_name)

        command = self.language_config.execution_command(
            cwd=working_directory,
            file=executable_name,
            dependencies=dependencies,
            arguments=[argument] if argument else []
        )
        logger.debug("Executing command %s in directory %s", command,
                     working_directory)
        # noinspection PyTypeChecker
        p = subprocess.run(command, input=stdin, text=True,
                           capture_output=True, cwd=working_directory)

        return BaseExecutionResult(
            stdout=p.stdout,
            stderr=p.stderr,
            exit=p.returncode
        )

    def compilation(self,
                    working_directory: Path,
                    dependencies: List[str]
                    ) -> Tuple[Optional[BaseExecutionResult], List[str]]:
        """
        The compilation step in the pipeline. This callback is used in both the
        precompilation and individual mode. The implementation may only depend on
        the arguments.

        In individual compilation mode, this function may be called in a multi-
        threaded environment. Since the implementation is obvious to which mode
        it is operating in, it must be thread-safe.

        In individual mode, this function is responsible for compiling the code,
        such that a single context can be executed for evaluation. The compilation
        happens for each context, just before execution.

        In precompilation mode, the function is responsible for compiling all code
        at once. In some languages, this means the compilation will fail if one
        context is not correct. For those languages, the judge will fallback to
        individual compilation. This fallback does come with a heavy execution speed
        penalty, so disabling the fallback if not needed is recommended.
        :param working_directory: The directory in which the dependencies are
                                  available and in which the compilation results
                                  should be stored.
        :param dependencies: A list of files available for compilation. Some
                             languages might need a main file. By convention, the
                             last file is the main file.
                             TODO: make this explicit?
        :return: A tuple containing an optional compilation result, and a list of
                 files, intended for further processing in the pipeline. For
                 languages without compilation, the dependencies can be returned
                 verbatim and without compilation results. Note that the judge might
                 decide to fallback to individual mode if the compilation result is
                 not positive.
        """
        command, files = self.language_config.generation_callback(dependencies)
        logger.debug("Generating files with command %s in directory %s",
                     command, working_directory)
        result = self.run_compilation_command(command, working_directory)

        return result, files

    def evaluate_custom(self,
                        evaluator: CustomEvaluator,
                        expected: Optional[Value],
                        actual: Optional[Value]) -> BaseExecutionResult:
        """
        Run the custom evaluation. Concerning structure and execution, the custom
        evaluator is very similar to the execution of the whole evaluation. It a
        mini-evaluation if you will.

        TODO: considering implementing a precompilation mode as well for custom
          evaluators. One difficulty is that there is currently no runtime support
          to decode values, only compile time support.
        """

        # Create a directory for this evaluator. If one exists, delete it first.
        evaluator_dir_name = humps.decamelize(evaluator.path.stem)
        custom_directory_name = f"{evaluator_dir_name}_{_get_identifier()}"
        custom_path = Path(self.config.workdir, "evaluators", custom_directory_name)
        if custom_path.exists():
            logger.debug("Removing existing directory for custom evaluator.")
            shutil.rmtree(custom_path, ignore_errors=True)
        custom_path.mkdir(parents=True)

        logger.info("Will do custom evaluation in %s", custom_path)

        # Get the language config & translator for the language of the evaluator.
        eval_judge = GeneratorJudge(self.config, self.out, evaluator.language)
        eval_lang_config = eval_judge.language_config
        eval_config = eval_judge.config
        eval_translator = eval_judge.translator

        # Copy the evaluator
        origin_path = Path(self.config.resources, evaluator.path)
        logger.debug("Copying %s to %s", origin_path, custom_path)
        shutil.copy2(origin_path, custom_path)

        # Copy the dependencies to the folder.
        dependencies = eval_lang_config.initial_dependencies()
        dependencies.extend(eval_lang_config.evaluator_dependencies())
        origin = path_to_templates(eval_lang_config, eval_config)
        utils.copy_from_paths_to_path(origin, dependencies, custom_path)
        # Include the actual evaluator in the dependencies.
        dependencies.append(evaluator.path.name)

        # Generate the evaluator.
        logger.debug("Generating custom evaluator.")
        evaluator_name = eval_translator.generate_custom_evaluator(
            destination=custom_path,
            evaluator=evaluator,
            actual=actual,
            expected=expected
        )
        dependencies.append(evaluator_name)
        logger.debug("Generated evaluator executor %s", evaluator_name)

        # Do compilation for those languages that require it.
        command, files = eval_lang_config.evaluator_generation_callback(
            dependencies
        )
        logger.debug("Compiling custom evaluator with command %s", command)
        result = eval_judge.run_compilation_command(command, custom_path)
        if result and result.stderr:
            raise TestPlanError(f"Error while compiling specific "
                                f"test case: {result.stderr}")

        # Execute the custom evaluator.
        evaluator_name = Path(evaluator_name).stem
        executable = eval_judge.find_main_file(files, evaluator_name)
        files.remove(executable)

        return eval_judge.execute_file(
            executable_name=executable,
            working_directory=custom_path,
            dependencies=files,
            stdin=None
        )
