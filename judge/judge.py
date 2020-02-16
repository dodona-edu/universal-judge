import logging
import shutil
import string
import subprocess
import tempfile
from dataclasses import replace
from multiprocessing.dummy import Pool
import random
from typing import Tuple

import utils
from dodona import *
from dodona import report_update, StartJudgment, StartTab, StartContext, \
    CloseContext, CloseTab, CloseJudgment
from evaluators import get_evaluator, Evaluator
from runners.runner import get_supporting_languages,  get_language_config
from runners.translator import Translator, TestcaseArguments, SelectorArguments, \
    MainTestcaseArguments, ContextArguments, CustomEvaluatorArguments
from serialisation import NothingType, SequenceType
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
    has_no_expected = expected_output == NoneChannelState.NONE or expected_output \
                      == IgnoredChannelState.IGNORED
    if is_correct and has_no_result and has_no_expected:
        return True

    report_update(out, StartTest(expected=evaluation_result.readable_expected))

    # Report any messages we received.
    for message in evaluation_result.messages:
        report_update(out, AppendMessage(message=message))

    # Close the test.
    report_update(out, CloseTest(generated=evaluation_result.readable_actual,
                                 status=status,
                                 data=TestData(channel=channel_name)))

    return is_correct


def _check_features(plan: Plan, language: str):
    """
    Check if a language supports all features needed by a certain testplan.
    In the future, we might also want to determine which languages support all
    required features,
    but that is currently not needed.
    :param plan:
    :param language: The programming language.
    """
    supported = get_supporting_languages(plan)
    if language not in supported:
        raise TestPlanError(
            f"The chosen language {language} does not support all required "
            f"features for the testplan.")


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

    # Report stdout.
    if results.stdout:
        # Append compiler messages to the output.
        messages.append("De compiler produceerde volgende uitvoer op stdout:")
        messages.append(ExtendedMessage(
            description=results.stdout,
            format='code'
        ))
        logger.debug("Received stdout from compiler: " + results.stderr)

    # Report stderr.
    if results.stderr:
        # Append compiler messages to the output.
        messages.append("De compiler produceerde volgende uitvoer op stderr:")
        messages.append(ExtendedMessage(
            description=results.stderr,
            format='code'
        ))
        logger.debug("Received stderr from compiler: " + results.stderr)

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
        self.config = config
        self.out = output
        if language is None:
            language = config.programming_language
        adjusted_config = replace(config, programming_language=language)
        language_config = get_language_config(language)
        self.language_config = get_language_config(config.programming_language)
        self.identifier = _get_identifier()
        self.translator = Translator(adjusted_config, language_config)

    def _process_results(self,
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
                stdout_evaluator = get_evaluator(
                    self.config,
                    testcase.output.stdout
                )
                stderr_evaluator = get_evaluator(
                    self.config,
                    testcase.output.stderr
                )
                file_evaluator = get_evaluator(
                    self.config,
                    testcase.output.file
                )
                value_evaluator = get_evaluator(
                    self.config,
                    testcase.output.result
                )
                exception_evaluator = get_evaluator(
                    self.config,
                    testcase.output.exception
                )
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

    def _check_required_features(self, plan: Plan) -> bool:
        """
        Check if the chosen language supports the required features for the given
        testplan. If the language supports all necessary features, this function
        just returns true. In other cases, the error is printed to stdout for
        Dodona and false is returned.
        """
        try:
            _check_features(plan, self.config.programming_language)
            return True
        except TestPlanError as e:
            report_update(self.out, StartJudgment())
            report_update(self.out, CloseJudgment(
                accepted=False,
                status=StatusMessage(
                    enum=Status.INTERNAL_ERROR,
                    human=str(e)
                )
            ))
            logger.info("Required features not supported: %s", e)
            return False

    def path_to_templates(self) -> List[Path]:
        """The path to the templates and normal files."""
        result = []
        for end in self.language_config.template_folders(
                self.config.programming_language):
            result.append(
                Path(self.config.judge) / 'judge' / 'runners' / 'templates' / end)
        return result

    def _generate(self, plan: Plan, mode: ExecutionMode) -> Tuple[Path, List[str]]:
        """
        Generate all necessary files, using the templates. This creates a common
        directory, copies all dependencies to that folder and runs the generation.
        """
        dependencies = self.language_config.initial_dependencies()
        common_dir = Path(self.config.workdir, f"common")
        common_dir.mkdir()

        # Copy dependencies
        dependency_paths = self.path_to_templates()
        utils.copy_from_paths_to_path(dependency_paths, dependencies, common_dir)

        # Copy the submission file.
        submission_name = f"{self.language_config.submission_name(plan)}" \
                          f".{self.language_config.file_extension()}"
        shutil.copy2(self.config.source, common_dir / submission_name)
        dependencies.append(submission_name)

        return common_dir, self.generation(
            plan=plan,
            working_directory=common_dir,
            dependencies=dependencies,
            mode=mode
        )

    def judge(self, plan: Plan):
        """
        Evaluate a solution for an exercise. Execute the tests present in the
        testplan. The result (the judgment) is sent to stdout, so Dodona can pick it
        up.
        :param plan: The plan to execute.
        """
        # Begin by checking if the given testplan is executable in this language.
        logger.info("Checking supported features...")
        if not self._check_required_features(plan):
            return  # Not all required features are supported.

        mode = plan.configuration.mode
        report_update(self.out, StartJudgment())

        logger.info("Start generating code...")
        common_dir, files = self._generate(plan, mode)

        if mode == ExecutionMode.PRECOMPILATION:
            # Compile all code in one go.
            logger.info("Running precompilation step...")
            result, compilation_files = self.compilation(common_dir, files)

            messages, status = _process_compile_results(result)
            precompilation_result = (messages, status)

            # If we have fallback, discard all results.
            if status != Status.CORRECT and plan.configuration.allow_fallback:
                mode = ExecutionMode.INDIVIDUAL
                logger.info("Compilation error, falling back to individual mode")
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
        pool = Pool(4 if plan.configuration.parallel else 1)

        with utils.protected_directory(common_dir) as common_dir:

            for tab_index, tab in enumerate(plan.tabs):
                report_update(self.out, StartTab(title=tab.name))
                # Create a list of arguments to execute (in threads)
                executions = []
                for context_index, context in enumerate(tab.contexts):
                    executions.append(ContextExecution(
                        context=context,
                        number=tab_index + context_index,
                        mode=mode,
                        common_directory=common_dir,
                        files=files,
                        precompilation_result=precompilation_result
                    ))

                # Do the executions in parallel
                results = pool.map(self._execute, executions)

                # Handle the results
                for context_index, context in enumerate(tab.contexts):
                    report_update(self.out, StartContext(
                        description=context.description
                    ))
                    execution_result, m, s = results[context_index]
                    self._process_results(
                        plan=plan,
                        context=context,
                        results=execution_result,
                        compiler_results=(m, s)
                    )
                    report_update(self.out, CloseContext())
                report_update(self.out, CloseTab())
            report_update(self.out, CloseJudgment())

    def _execute(self,
                 args: ContextExecution
                 ) -> Tuple[Optional[ExecutionResult], List[Message], Status]:
        """
        Execute a context.
        """
        # Create a working directory for the context.
        context_directory = Path(self.config.workdir, f"context-{args.number}")
        context_directory.mkdir()

        logger.info("Executing context %d in path %s",
                    args.number, context_directory)

        # Copy files from the common directory to the context directory.
        for file in args.files:
            origin = args.common_directory / file
            logger.debug("Copying %s to %s", origin, context_directory)
            # noinspection PyTypeChecker
            shutil.copy2(origin, context_directory)

        # If needed, do a compilation.
        if args.mode == ExecutionMode.INDIVIDUAL:
            logger.info("Compiling context %d in INDIVIDUAL mode...", args.number)
            result, files = self.compilation(
                working_directory=context_directory,
                dependencies=args.files
            )

            # Process compilation results.
            messages, status = _process_compile_results(result)

            if status != Status.CORRECT:
                logger.debug("Compilation of individual context failed.")
                logger.debug("Aborting executing of this context.")
                return None, messages, status

            logger.debug("Executing context %d in INDIVIDUAL mode...", args.number)
            # Execute the individual files.
            context_name = self.language_config.context_name(args.context)
            executable = self.find_main_file(files, context_name)
            files.remove(executable)
            stdin = args.context.get_stdin(self.config.resources)

            result = self.execute(
                executable_file=executable,
                working_directory=context_directory,
                dependencies=files,
                stdin=stdin
            )
            return result, messages, status
        else:
            result, files = None, args.files

            logger.info("Executing context %d in PRECOMPILATION mode...",
                        args.number)
            selector_name = self.language_config.selector_name()
            executable = self.find_main_file(files, selector_name)
            files.remove(executable)
            stdin = args.context.get_stdin(self.config.resources)

            result = self.execute(
                executable_file=executable,
                working_directory=context_directory,
                dependencies=files,
                stdin=stdin,
                context_argument=str(args.number)
            )
            return result, [], Status.CORRECT

    def find_main_file(self, files: List[str], name: str) -> str:
        return [x for x in files if x.startswith(name)][0]

    def _value_file(self, working_directory: Path):
        return working_directory / f"{self.identifier}_values.txt"

    def _exception_file(self, working_directory: Path):
        return working_directory / f"{self.identifier}_exceptions.txt"

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

    def _compile(self, command: List[str], working_directory) \
            -> Optional[BaseExecutionResult]:
        if command:
            p = subprocess.run(command, text=True, capture_output=True,
                               cwd=working_directory)
            return BaseExecutionResult(p.stdout, p.stderr, p.returncode)
        else:
            return None

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
        command, files = self.language_config.generation_callback(dependencies)
        logger.debug("Generating files with command %s in directory %s",
                     command, working_directory)
        result = self._compile(command, working_directory)

        return result, files

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
            origin = self.translator.path_to_templates()
            utils.copy_from_paths_to_path(origin, dependencies, directory)

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
