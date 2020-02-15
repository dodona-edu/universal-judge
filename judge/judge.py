import logging
import shutil
from multiprocessing.dummy import Pool
from typing import Tuple

import utils
from dodona import *
from dodona import report_update, StartJudgment, StartTab, StartContext, \
    CloseContext, CloseTab, CloseJudgment
from evaluators import get_evaluator, Evaluator
from runners.runner import SupportsRunner, ExecutionResult, get_generator, \
    BaseExecutionResult, get_supporting_languages, \
    ContextExecution, get_language_config
from tested import Config
from testplan import *

logger = logging.getLogger(__name__)


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
    runner: SupportsRunner

    def __init__(self, config: Config, output: IO):
        self.config = config
        self.out = output
        self.language_config = get_language_config(config.programming_language)
        self.runner = get_generator(config)

    def _process_compile_results(self,
                                 results: Optional[BaseExecutionResult]
                                 ) -> Tuple[List[Message], Status]:
        """Process compilation output."""

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

    def _process_results(self,
                         plan: Plan,
                         context: Context,
                         results: ExecutionResult,
                         compiler_results: Tuple[List[Message], Status]):
        # Process output
        stdout_ = results.stdout.split(results.separator)\
            if results is not None else []
        stderr_ = results.stderr.split(results.separator)\
            if results is not None else []
        values = results.results.split(results.separator)\
            if results is not None else []
        exceptions = results.exceptions.split(results.separator)\
            if results is not None else []

        # There might be less output than testcase, which is an error. However,
        # we process the
        # output we have, to ensure we send as much feedback as possible to the
        # user.
        testcase: Testcase  # Type hint for pycharm.
        for i, testcase in enumerate(context.all_testcases()):

            name = self.language_config.submission_name(plan)
            readable_input = self.runner.get_readable_input(name, testcase)
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
                stdout_evaluator = get_evaluator(self.config,
                                                 testcase.output.stdout)
                stderr_evaluator = get_evaluator(self.config,
                                                 testcase.output.stderr)
                file_evaluator = get_evaluator(self.config, testcase.output.file)
                value_evaluator = get_evaluator(self.config, testcase.output.result)
                exception_evaluator = get_evaluator(self.config,
                                                    testcase.output.exception)
            except TestPlanError as e:
                report_update(self.out, AppendMessage(message=ExtendedMessage(
                    description=str(e),
                    format='text',
                    permission=Permission.STAFF
                )))
                break

            # Evaluate the file channel.
            results = [
                _evaluate_channel(self.out, "file", testcase.output.file, None,
                                  file_evaluator)]

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
                                  actual_value, value_evaluator))

            # Check if there is early termination.
            if i >= len(stdout_) or i >= len(stderr_) or i >= len(
                    values) or i >= len(exceptions):
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

        return common_dir, self.runner.generation(
            plan=plan,
            working_directory=common_dir,
            dependencies=dependencies,
            mode=mode
        )

    def _precompile(self,
                    common_dir: Path,
                    generated_files: List[str]
                    ) -> Tuple[Optional[BaseExecutionResult], List[str]]:
        """
        Run the precompilation step.
        """
        return self.runner.compilation(common_dir, generated_files)

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
            result, compilation_files = self._precompile(common_dir, files)

            messages, status = self._process_compile_results(result)
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
                    report_update(self.out,StartContext(
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
            result, files = self.runner.compilation(
                working_directory=context_directory,
                dependencies=args.files
            )

            # Process compilation results.
            messages, status = self._process_compile_results(result)

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

            result = self.runner.execute(
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

            result = self.runner.execute(
                executable_file=executable,
                working_directory=context_directory,
                dependencies=files,
                stdin=stdin,
                context_argument=str(args.number)
            )
            return result, [], Status.CORRECT

    def find_main_file(self, files: List[str], name: str) -> str:
        return [x for x in files if x.startswith(name)][0]
