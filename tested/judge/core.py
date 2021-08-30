import concurrent
import logging
import shutil
import time
from concurrent.futures.thread import ThreadPoolExecutor
from pathlib import Path
from typing import Tuple, List

from .collector import OutputManager
from .compilation import run_compilation, process_compile_results
from .evaluation import evaluate_run_results, \
    evaluate_context_results
from .execution import Execution, execute_execution, ExecutionResult
from tested.internal_timings import new_stage, end_stage, is_collecting, \
    pretty_print_timings
from .linter import run_linter
from .utils import copy_from_paths_to_path, BaseExecutionResult
from ..configs import Bundle
from ..dodona import *
from ..features import is_supported
from ..languages.generator import generate_execution, generate_selector
from ..languages.templates import path_to_templates
from ..testplan import ExecutionMode
from ..internationalization import set_locale, get_i18n_string

_logger = logging.getLogger(__name__)


def judge(bundle: Bundle):
    """
    Evaluate a solution for an exercise. Execute the tests present in the
    testplan. The result (the judgment) is sent to stdout, so Dodona can pick it
    up.

    :param bundle: The configuration bundle.
    """
    new_stage("analyse.supported")
    # Begin by checking if the given testplan is executable in this language.
    _logger.info("Checking supported features...")
    set_locale(bundle.config.natural_language)
    if not is_supported(bundle):
        end_stage("analyse.supported")
        report_update(bundle.out, StartJudgment())
        report_update(bundle.out, CloseJudgment(
            accepted=False,
            status=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human=get_i18n_string("judge.core.unsupported.language",
                                      language=bundle.config.programming_language)
            )
        ))
        _logger.info("Required features not supported.")
        return  # Not all required features are supported.

    new_stage("prepare.output")
    collector = OutputManager(bundle)
    collector.add(StartJudgment())

    max_time = float(bundle.config.time_limit) * 0.9
    start = time.perf_counter()

    # Run the linter.
    new_stage("linter")
    run_linter(bundle, collector, max_time)
    if time.perf_counter() - start > max_time:
        collector.terminate(Status.TIME_LIMIT_EXCEEDED)
        return

    if bundle.plan.is_io_only():
        cont = _judge_io(bundle, collector, max_time, start)
    else:
        cont = _judge(bundle, collector, max_time, start)

    # Terminate if judge may not continue
    if not cont:
        return

    # Add statistics tab for STAFF
    if is_collecting():
        collector.add_tab(
            StartTab(title=get_i18n_string("timings.title"),
                     permission=Permission.STAFF), -1)
        collector.add(AppendMessage(message=ExtendedMessage(
            description=pretty_print_timings(bundle.config.options.parallel),
            format="markdown",
            permission=Permission.STAFF
        )))
        collector.add_tab(CloseTab(), -1)

    collector.add(CloseJudgment())
    collector.clean_finish()


def _judge(bundle: Bundle, collector: OutputManager, max_time: float,
           start: float) -> bool:
    mode = bundle.config.options.mode
    _logger.info("Start generating code...")
    new_stage("generation")
    common_dir, files, selector = _generate_files(bundle, mode)
    # Add the selector to the dependencies.
    if selector:
        files.append(selector)

    if mode == ExecutionMode.PRECOMPILATION:
        new_stage("compilation.pre")
        assert not bundle.lang_config.needs_selector() or selector is not None
        files = _copy_workdir_source_files(bundle, common_dir) + files

        # Compile all code in one go.
        _logger.info("Running precompilation step...")
        remaining = max_time - (time.perf_counter() - start)
        result, compilation_files = run_compilation(bundle, common_dir, files,
                                                    remaining)

        messages, status, annotations = process_compile_results(
            bundle.plan.namespace,
            bundle.lang_config,
            result
        )

        # If there is no result, there was no compilation.
        if not result:
            precompilation_result = None
        else:
            # Handle timout if necessary.
            if result.timeout or result.memory:
                _handle_out_of_memory_timeout(bundle, collector, annotations,
                                              messages, result)
                return False

            assert not result.timeout
            assert not result.memory

            precompilation_result = (messages, status)

            # If we have fallback, discard all results.
            if status != Status.CORRECT and bundle.config.options.allow_fallback:
                mode = ExecutionMode.INDIVIDUAL
                _logger.info("Compilation error, falling back to individual mode")
                # Remove the selector file from the dependencies.
                # Otherwise, it will keep being compiled, which we want to avoid.
                if bundle.lang_config.needs_selector():
                    files.remove(selector)
            # When compilation succeeded, only add annotations
            elif status == Status.CORRECT:
                files = compilation_files
                for annotation in annotations:
                    collector.add(annotation)
            # Add compilation tab when compilation failed
            else:
                # Report messages.
                if messages:
                    collector.add_tab(
                        StartTab(get_i18n_string("judge.core.compilation")), -1)
                for message in messages:
                    collector.add(AppendMessage(message=message))
                for annotation in annotations:
                    collector.add(annotation)
                if messages:
                    collector.add_tab(CloseTab(), -1)

                collector.terminate(StatusMessage(
                    enum=status,
                    human=get_i18n_string("judge.core.invalid.source-code")
                ))
                _logger.info("Compilation error without fallback")
                return False  # Compilation error occurred, useless to continue.
        end_stage("compilation.pre")
    else:
        precompilation_result = None
        end_stage("generation")

    _logger.info("Starting judgement...")
    parallel = bundle.config.options.parallel
    # How much of the output limit we still have.
    output_limit = bundle.config.output_limit * 0.8

    # Create a list of runs we want to execute.
    for tab_index, tab in enumerate(bundle.plan.tabs):
        collector.add_tab(StartTab(title=tab.name, hidden=tab.hidden), tab_index)
        executions = []
        offset = 0
        for execution_index, run in enumerate(tab.runs):
            executions.append(Execution(
                run=run,
                context_offset=offset,
                execution_name=bundle.lang_config.execution_name(
                    tab_number=tab_index,
                    execution_number=execution_index
                ),
                execution_index=execution_index,
                mode=mode,
                common_directory=common_dir,
                files=files,
                precompilation_result=precompilation_result,
                collector=collector
            ))
            offset += int(run.run.input.main_call) + len(run.contexts)

        remaining = max_time - (time.perf_counter() - start)
        if parallel:
            result = _parallel_execution(bundle, executions, remaining)
        else:
            result = _single_execution(bundle, executions, remaining)

        if result in (Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED,
                      Status.OUTPUT_LIMIT_EXCEEDED):
            assert not collector.collected
            collector.terminate(result)
            return False
        collector.add_tab(CloseTab(), tab_index)
    return True


def _judge_io(bundle: Bundle, collector: OutputManager, max_time: float,
              start: float):
    _logger.info("Copy all dependencies...")
    new_stage("dependencies.copy")
    common_dir = Path(bundle.config.workdir, f"common")
    common_dir.mkdir()

    # Submission filename
    submission_name = bundle.lang_config.submission_name(bundle.plan)

    # Copy the submission file.
    submission_file = f"{submission_name}" \
                      f".{bundle.lang_config.extension_file()}"
    solution_path = common_dir / submission_file
    # noinspection PyTypeChecker
    shutil.copy2(bundle.config.source, solution_path)
    files = [submission_file]

    # Copy workdir source files
    files.extend(_copy_workdir_source_files(bundle, common_dir))

    # Compilation
    new_stage("compilation.pre")

    # Compile all code in one go.
    _logger.info("Running precompilation step...")
    remaining = max_time - (time.perf_counter() - start)
    result, compilation_files = run_compilation(bundle, common_dir, files,
                                                remaining)

    messages, status, annotations = process_compile_results(
        bundle.plan.namespace,
        bundle.lang_config,
        result
    )

    # If there is no result, there was no compilation.
    if result:
        # Handle timout if necessary.
        if result.timeout or result.memory:
            _handle_out_of_memory_timeout(bundle, collector, annotations, messages,
                                          result)
            return False

        assert not result.timeout
        assert not result.memory

        precompilation_result = (messages, status)

        # When compilation succeeded, only add annotations
        if status == Status.CORRECT:
            files = compilation_files
            for annotation in annotations:
                collector.add(annotation)
        # Add compilation tab when compilation failed
        else:
            # Report messages.
            collector.add_tab(StartTab(get_i18n_string("judge.core.compilation")),
                              -1)
            for message in messages:
                collector.add(AppendMessage(message=message))
            for annotation in annotations:
                collector.add(annotation)

            collector.add_tab(CloseTab(), -1)

            collector.terminate(StatusMessage(
                enum=status,
                human=get_i18n_string("judge.core.invalid.source-code")
            ))
            _logger.info("Compilation error without fallback")
            return False  # Compilation error occurred, useless to continue.
    end_stage("compilation.pre")
    # Compilation done
    pass
    # Succesfull terminated
    return True


def _handle_out_of_memory_timeout(bundle: Bundle, collector: OutputManager,
                                  annotations: List[AnnotateCode],
                                  messages: List[Message],
                                  result: BaseExecutionResult):
    # Show in separate tab.
    index = len(bundle.plan.tabs) + 1
    if messages:
        collector.prepare_tab(StartTab(get_i18n_string("judge.core.compilation")),
                              index)
    for message in messages:
        collector.add(AppendMessage(message=message))
    for annotation in annotations:
        collector.add(annotation)
    if messages:
        collector.prepare_tab(CloseTab(), index)
    collector.terminate(
        Status.TIME_LIMIT_EXCEEDED if result.timeout else
        Status.MEMORY_LIMIT_EXCEEDED)


def _single_execution(bundle: Bundle,
                      items: List[Execution],
                      max_time: float) -> Optional[Status]:
    """
    Process items in a non-threaded way.

    :param bundle: The configuration bundle.
    :param items: The contexts to execute.
    :param max_time: The max amount of time.
    """
    start = time.perf_counter()
    for execution in items:
        remaining = max_time - (time.perf_counter() - start)
        new_stage("run.execution")
        execution_result, m, s, p = execute_execution(bundle, execution, remaining)

        new_stage("evaluate.results")
        status = _process_results(bundle, execution, execution_result, m, s, p)
        end_stage("evaluate.results")

        if status:
            return status


def _parallel_execution(bundle: Bundle,
                        items: List[Execution],
                        max_time: float) -> Optional[Status]:
    """
    Execute a list of contexts in parallel.

    :param bundle: The configuration bundle.
    :param items: The contexts to execute.
    :param max_time: The max amount of time.
    """
    start = time.perf_counter()  # Accessed from threads.

    def threaded_execution(execution: Execution):
        """The function executed in parallel."""
        remainder = max_time - (time.perf_counter() - start)
        new_stage("run.execution")
        execution_result, m, s, p = execute_execution(bundle, execution, remainder)
        end_stage("run.execution")

        def evaluation_function(eval_remainder):
            new_stage("evaluate.results")
            _status = _process_results(bundle, execution, execution_result, m, s, p)
            end_stage("evaluate.results")

            if _status and _status not in (Status.TIME_LIMIT_EXCEEDED,
                                           Status.MEMORY_LIMIT_EXCEEDED):
                return _status

        return evaluation_function

    with ThreadPoolExecutor(max_workers=4) as executor:
        remaining = max_time - (time.perf_counter() - start)
        results = executor.map(threaded_execution, items, timeout=remaining)
        try:
            for eval_function in list(results):
                remaining = max_time - (time.perf_counter() - start)
                if (status := eval_function(remaining)) in (
                        Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED,
                        Status.OUTPUT_LIMIT_EXCEEDED):
                    # Ensure finally is called NOW and cancels remaining tasks.
                    del results
                    return status
        except concurrent.futures.TimeoutError:
            _logger.warning("Futures did not end soon enough.", exc_info=True)
            return Status.TIME_LIMIT_EXCEEDED


def _generate_files(bundle: Bundle,
                    mode: ExecutionMode
                    ) -> Tuple[Path, List[str], Optional[str]]:
    """
    Generate all necessary files, using the templates. This creates a common
    directory, copies all dependencies to that folder and runs the generation.
    """
    dependencies = bundle.lang_config.initial_dependencies()
    common_dir = Path(bundle.config.workdir, f"common")
    common_dir.mkdir()

    _logger.debug(f"Generating files in common directory %s", common_dir)

    # Copy dependencies
    dependency_paths = path_to_templates(bundle)
    copy_from_paths_to_path(dependency_paths, dependencies, common_dir)

    submission_name = bundle.lang_config.submission_name(bundle.plan)

    # Copy the submission file.
    submission_file = f"{submission_name}" \
                      f".{bundle.lang_config.extension_file()}"
    solution_path = common_dir / submission_file
    # noinspection PyTypeChecker
    shutil.copy2(bundle.config.source, solution_path)
    dependencies.append(submission_file)

    # Allow modifications of the submission file.
    new_stage("submission.modify", sub_stage=True)
    bundle.lang_config.solution(solution_path, bundle)

    # The names of the executions for the testplan.
    new_stage("generate.templates", sub_stage=True)
    execution_names = []
    # Generate the files for each execution.
    for tab_i, tab in enumerate(bundle.plan.tabs):
        for run_i, run in enumerate(tab.runs):
            execution_name = bundle.lang_config.execution_name(tab_i, run_i)
            _logger.debug(f"Generating file for execution {execution_name}")
            generated, evaluators = generate_execution(
                bundle=bundle,
                destination=common_dir,
                run=run,
                execution_name=execution_name
            )
            # Copy evaluators to the directory.
            for evaluator in evaluators:
                source = Path(bundle.config.resources) / evaluator
                _logger.debug("Copying evaluator from %s to %s",
                              source, common_dir)
                shutil.copy2(source, common_dir)
            dependencies.extend(evaluators)
            dependencies.append(generated)
            execution_names.append(execution_name)

    if mode == ExecutionMode.PRECOMPILATION \
            and bundle.lang_config.needs_selector():
        _logger.debug("Generating selector for PRECOMPILATION mode.")
        generated = generate_selector(bundle, common_dir, execution_names)
    else:
        generated = None
    return common_dir, dependencies, generated


def _process_results(bundle: Bundle, execution: Execution,
                     execution_result: Optional[ExecutionResult], m: List[Message],
                     s: Status, p: Path) -> Optional[Status]:
    if execution_result:
        context_results, run_testcase = \
            execution_result.to_context_execution_results()
    else:
        context_results, run_testcase = [None] * len(execution.run.contexts), None

    has_main = execution.run.run.input.main_call
    exit_code = execution.run.run.output.exit_code

    if has_main or (len(context_results) == 0 and (
            run_testcase.exit or run_testcase.stdout or run_testcase.stderr or
            run_testcase.exception or run_testcase.timeout or run_testcase.memory)):

        offset = execution.context_offset if has_main else None
        if not has_main:
            execution.run.run.description = get_i18n_string(
                "judge.core.initialization")
        execution.collector.add_context(
            StartContext(),
            offset
        )
        continue_ = evaluate_run_results(bundle, execution.run.run, run_testcase,
                                         (m, s), p, execution.collector,
                                         bool(context_results))
        execution.collector.add_context(CloseContext(), offset)
        if execution.collector.is_full():
            return Status.OUTPUT_LIMIT_EXCEEDED
        if continue_ in (Status.TIME_LIMIT_EXCEEDED,
                         Status.MEMORY_LIMIT_EXCEEDED):
            return continue_

    for index, (context, context_result) in enumerate(
            zip(execution.run.contexts, context_results),
            int(has_main) + execution.context_offset):
        execution.collector.add_context(
            StartContext(description=context.description), index
        )

        if index == 0 and not has_main:
            mi = m
        else:
            mi = []

        continue_ = evaluate_context_results(bundle, context=context,
                                             exec_results=context_result,
                                             compiler_results=(mi, s),
                                             context_dir=p,
                                             collector=execution.collector,
                                             exit_output=exit_code)
        execution.collector.add_context(CloseContext(), index)
        if execution.collector.is_full():
            return Status.OUTPUT_LIMIT_EXCEEDED
        if continue_ in (Status.TIME_LIMIT_EXCEEDED,
                         Status.MEMORY_LIMIT_EXCEEDED):
            return continue_


def _copy_workdir_source_files(bundle: Bundle, common_dir: Path) -> List[str]:
    """
    Copy additional source files from the workdir to the common dir

    :param bundle: Bundle information of the test plan
    :param common_dir: The directory of the other files
    """
    prefix = bundle.lang_config.execution_prefix()
    source_files = []

    def recursive_copy(src: Path, dst: Path):
        for origin in src.iterdir():
            file = origin.name.lower()
            if origin.is_file() and bundle.lang_config.is_source_file(origin):
                source_files.append(str(dst / origin.name))
                _logger.debug("Copying %s to %s", origin, dst)
                shutil.copy2(origin, dst)
            elif origin.is_dir() and not file.startswith(
                    prefix) and file != "common":
                _logger.debug("Iterate subdir %s", dst / file)
                shutil.copytree(origin, dst / file)

    recursive_copy(bundle.config.workdir, common_dir)

    return source_files
