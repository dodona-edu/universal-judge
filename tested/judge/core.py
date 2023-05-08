import concurrent
import logging
import shutil
import time
from concurrent.futures.thread import ThreadPoolExecutor
from pathlib import Path
from typing import List, Optional, Tuple

from tested.configs import Bundle
from tested.dodona import (
    AppendMessage,
    CloseContext,
    CloseJudgment,
    CloseTab,
    Message,
    StartContext,
    StartJudgment,
    StartTab,
    Status,
    StatusMessage,
    report_update,
)
from tested.features import is_supported
from tested.internationalization import get_i18n_string, set_locale
from tested.judge.collector import OutputManager
from tested.judge.compilation import process_compile_results, run_compilation
from tested.judge.evaluation import evaluate_context_results
from tested.judge.execution import (
    Execution,
    ExecutionResult,
    execute_execution,
    merge_contexts_into_units,
)
from tested.judge.linter import run_linter
from tested.judge.utils import copy_from_paths_to_path
from tested.languages.conventionalize import submission_file
from tested.languages.generation import generate_execution, generate_selector
from tested.languages.templates import path_to_dependencies
from tested.testsuite import ExecutionMode

_logger = logging.getLogger(__name__)


def judge(bundle: Bundle):
    """
    Evaluate a solution for an exercise. Execute the tests present in the
    test suite. The result (the judgment) is sent to stdout, so Dodona can pick it
    up.

    :param bundle: The configuration bundle.
    """
    # Begin by checking if the given test suite is executable in this language.
    _logger.info("Checking supported features...")
    set_locale(bundle.config.natural_language)
    if not is_supported(bundle):
        report_update(bundle.out, StartJudgment())
        report_update(
            bundle.out,
            CloseJudgment(
                accepted=False,
                status=StatusMessage(
                    enum=Status.INTERNAL_ERROR,
                    human=get_i18n_string(
                        "judge.core.unsupported.language",
                        language=bundle.config.programming_language,
                    ),
                ),
            ),
        )
        _logger.info("Required features not supported.")
        return  # Not all required features are supported.

    mode = bundle.config.options.mode
    collector = OutputManager(bundle)
    collector.add(StartJudgment())

    max_time = float(bundle.config.time_limit) * 0.9
    start = time.perf_counter()

    # Run the linter.
    run_linter(bundle, collector, max_time)
    if time.perf_counter() - start > max_time:
        collector.terminate(Status.TIME_LIMIT_EXCEEDED)
        return

    _logger.info("Start generating code...")
    common_dir, files, selector = _generate_files(bundle, mode)
    # Add the selector to the dependencies.
    if selector:
        files.append(selector)

    if mode == ExecutionMode.PRECOMPILATION:
        assert not bundle.lang_config.needs_selector() or selector is not None
        files = _copy_workdir_source_files(bundle, common_dir) + files

        # Compile all code in one go.
        _logger.info("Running precompilation step...")
        remaining = max_time - (time.perf_counter() - start)
        result, compilation_files = run_compilation(
            bundle, common_dir, files, remaining
        )

        messages, status, annotations = process_compile_results(
            bundle.suite.namespace, bundle.lang_config, result
        )

        # If there is no result, there was no compilation.
        if not result:
            precompilation_result = None
        else:
            # Handle timout if necessary.
            if result.timeout or result.memory:
                # Show in separate tab.
                index = len(bundle.suite.tabs) + 1
                if messages:
                    collector.prepare_tab(
                        StartTab(get_i18n_string("judge.core.compilation")), index
                    )
                for message in messages:
                    collector.add(AppendMessage(message=message))
                for annotation in annotations:
                    collector.add(annotation)
                if messages:
                    collector.prepare_tab(CloseTab(), index)
                collector.terminate(
                    Status.TIME_LIMIT_EXCEEDED
                    if result.timeout
                    else Status.MEMORY_LIMIT_EXCEEDED
                )
                return

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
                        StartTab(get_i18n_string("judge.core.compilation")), -1
                    )
                for message in messages:
                    collector.add(AppendMessage(message=message))
                for annotation in annotations:
                    collector.add(annotation)
                if messages:
                    collector.add_tab(CloseTab(), -1)

                collector.terminate(
                    StatusMessage(
                        enum=status,
                        human=get_i18n_string("judge.core.invalid.source-code"),
                    )
                )
                _logger.info("Compilation error without fallback")
                return  # Compilation error occurred, useless to continue.
    else:
        precompilation_result = None

    _logger.info("Starting judgement...")
    parallel = bundle.config.options.parallel

    # Create a list of runs we want to execute.
    for tab_index, tab in enumerate(bundle.suite.tabs):
        collector.add_tab(StartTab(title=tab.name, hidden=tab.hidden), tab_index)
        execution_units = merge_contexts_into_units(tab.contexts)
        executions = []
        offset = 0
        for execution_index, unit in enumerate(execution_units):
            executions.append(
                Execution(
                    unit=unit,
                    context_offset=offset,
                    execution_name=bundle.lang_config.execution_name(
                        tab_number=tab_index, execution_number=execution_index
                    ),
                    execution_index=execution_index,
                    mode=mode,
                    common_directory=common_dir,
                    files=files,
                    precompilation_result=precompilation_result,
                    collector=collector,
                )
            )
            offset += len(unit.contexts)

        remaining = max_time - (time.perf_counter() - start)
        if parallel:
            result = _parallel_execution(bundle, executions, remaining)
        else:
            result = _single_execution(bundle, executions, remaining)

        if result in (
            Status.TIME_LIMIT_EXCEEDED,
            Status.MEMORY_LIMIT_EXCEEDED,
            Status.OUTPUT_LIMIT_EXCEEDED,
        ):
            assert not collector.collected
            collector.terminate(result)
            return
        collector.add_tab(CloseTab(), tab_index)

    collector.add(CloseJudgment())
    collector.clean_finish()


def _single_execution(
    bundle: Bundle, items: List[Execution], max_time: float
) -> Optional[Status]:
    """
    Process items in a non-threaded way.

    :param bundle: The configuration bundle.
    :param items: The contexts to execute.
    :param max_time: The max amount of time.
    """
    start = time.perf_counter()
    for execution in items:
        remaining = max_time - (time.perf_counter() - start)
        execution_result, m, s, p = execute_execution(bundle, execution, remaining)

        status = _process_results(bundle, execution, execution_result, m, s, p)

        if status:
            return status
    return None


def _parallel_execution(
    bundle: Bundle, items: List[Execution], max_time: float
) -> Optional[Status]:
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
        execution_result, m, s, p = execute_execution(bundle, execution, remainder)

        def evaluation_function(_eval_remainder):
            _status = _process_results(bundle, execution, execution_result, m, s, p)

            if _status and _status not in (
                Status.TIME_LIMIT_EXCEEDED,
                Status.MEMORY_LIMIT_EXCEEDED,
            ):
                return _status
            return None

        return evaluation_function

    with ThreadPoolExecutor(max_workers=4) as executor:
        remaining = max_time - (time.perf_counter() - start)
        results = executor.map(threaded_execution, items, timeout=remaining)
        try:
            for eval_function in list(results):
                remaining = max_time - (time.perf_counter() - start)
                if (status := eval_function(remaining)) in (
                    Status.TIME_LIMIT_EXCEEDED,
                    Status.MEMORY_LIMIT_EXCEEDED,
                    Status.OUTPUT_LIMIT_EXCEEDED,
                ):
                    # Ensure finally is called NOW and cancels remaining tasks.
                    del results
                    return status
        except concurrent.futures.TimeoutError:
            _logger.warning("Futures did not end soon enough.", exc_info=True)
            return Status.TIME_LIMIT_EXCEEDED


def _generate_files(
    bundle: Bundle, mode: ExecutionMode
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
    dependency_paths = path_to_dependencies(bundle)
    copy_from_paths_to_path(dependency_paths, dependencies, common_dir)

    # Copy the submission file.
    submission = submission_file(bundle.lang_config, bundle.suite)
    solution_path = common_dir / submission
    # noinspection PyTypeChecker
    shutil.copy2(bundle.config.source, solution_path)
    dependencies.append(submission)

    # Allow modifications of the submission file.
    bundle.lang_config.modify_solution(solution_path)

    # The names of the executions for the test suite.
    execution_names = []
    # Generate the files for each execution.
    for tab_i, tab in enumerate(bundle.suite.tabs):
        execution_units = merge_contexts_into_units(tab.contexts)
        for unit_i, unit in enumerate(execution_units):
            execution_name = bundle.lang_config.execution_name(tab_i, unit_i)
            _logger.debug(f"Generating file for execution {execution_name}")
            generated, evaluators = generate_execution(
                bundle=bundle,
                destination=common_dir,
                execution_unit=unit,
                execution_name=execution_name,
            )
            # Copy evaluators to the directory.
            for evaluator in evaluators:
                source = Path(bundle.config.resources) / evaluator
                _logger.debug("Copying evaluator from %s to %s", source, common_dir)
                shutil.copy2(source, common_dir)
            dependencies.extend(evaluators)
            dependencies.append(generated)
            execution_names.append(execution_name)

    if mode == ExecutionMode.PRECOMPILATION and bundle.lang_config.needs_selector():
        _logger.debug("Generating selector for PRECOMPILATION mode.")
        generated = generate_selector(bundle, common_dir, execution_names)
    else:
        generated = None
    return common_dir, dependencies, generated


def _process_results(
    bundle: Bundle,
    execution: Execution,
    execution_result: Optional[ExecutionResult],
    compiler_messages: List[Message],
    s: Status,
    p: Path,
) -> Optional[Status]:
    if execution_result:
        context_results = execution_result.to_context_results()
    else:
        context_results = [None] * len(execution.unit.contexts)

    for index, (context, context_result) in enumerate(
        zip(execution.unit.contexts, context_results), execution.context_offset
    ):
        execution.collector.add_context(
            StartContext(description=context.description), index
        )

        continue_ = evaluate_context_results(
            bundle,
            context=context,
            exec_results=context_result,
            compiler_results=(compiler_messages, s),
            context_dir=p,
            collector=execution.collector,
        )

        # We handled the compiler messages above, so remove them.
        compiler_messages = []

        execution.collector.add_context(CloseContext(), index)
        if execution.collector.is_full():
            return Status.OUTPUT_LIMIT_EXCEEDED
        if continue_ in (Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED):
            return continue_
    return None


def _copy_workdir_source_files(bundle: Bundle, common_dir: Path) -> List[str]:
    """
    Copy additional source files from the workdir to the common dir

    :param bundle: Bundle information of the test suite
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
            elif origin.is_dir() and not file.startswith(prefix) and file != "common":
                _logger.debug("Iterate subdir %s", dst / file)
                shutil.copytree(origin, dst / file)

    recursive_copy(bundle.config.workdir, common_dir)

    return source_files
