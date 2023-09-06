import logging
import shutil
import time
from pathlib import Path
from typing import List, Optional, Tuple

from tested.configs import Bundle
from tested.dodona import (
    CloseContext,
    CloseJudgement,
    CloseTab,
    Message,
    StartContext,
    StartJudgement,
    StartTab,
    Status,
    StatusMessage,
    report_update,
)
from tested.features import is_supported
from tested.internationalization import get_i18n_string, set_locale
from tested.judge.collector import OutputManager
from tested.judge.compilation import process_compile_results, run_compilation
from tested.judge.evaluation import evaluate_context_results, terminate
from tested.judge.execution import (
    Execution,
    ExecutionResult,
    execute_execution,
    merge_contexts_into_units,
)
from tested.judge.linter import run_linter
from tested.judge.utils import copy_from_paths_to_path
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    execution_name,
    submission_file,
)
from tested.languages.generation import generate_execution, generate_selector
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
    if not is_supported(bundle.lang_config):
        report_update(bundle.out, StartJudgement())
        report_update(
            bundle.out,
            CloseJudgement(
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
    collector = OutputManager(bundle.out)
    collector.add(StartJudgement())

    max_time = float(bundle.config.time_limit) * 0.9
    start = time.perf_counter()

    # Run the linter.
    run_linter(bundle, collector, max_time)
    if time.perf_counter() - start > max_time:
        terminate(bundle, collector, Status.TIME_LIMIT_EXCEEDED)
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
            bundle.lang_config, result
        )

        # If there is no result, there was no compilation.
        if not result:
            precompilation_result = None
        else:
            # Handle timout if necessary.
            if result.timeout or result.memory:
                collector.add_messages(messages)
                collector.add_all(annotations)
                status = (
                    Status.TIME_LIMIT_EXCEEDED
                    if result.timeout
                    else Status.MEMORY_LIMIT_EXCEEDED
                )
                terminate(bundle, collector, status)
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
                if selector and bundle.lang_config.needs_selector():
                    files.remove(selector)
            # When compilation succeeded, only add annotations
            elif status == Status.CORRECT:
                files = compilation_files
                collector.add_all(annotations)
            else:
                collector.add_messages(messages)
                collector.add_all(annotations)

                terminate(
                    bundle,
                    collector,
                    StatusMessage(
                        enum=status,
                        human=get_i18n_string("judge.core.invalid.source-code"),
                    ),
                )
                _logger.info("Compilation error without fallback")
                return  # Compilation error occurred, useless to continue.
    else:
        precompilation_result = None

    _logger.info("Starting judgement...")
    # Create a list of runs we want to execute.
    for tab_index, tab in enumerate(bundle.suite.tabs):
        collector.add(StartTab(title=tab.name, hidden=tab.hidden))
        assert tab.contexts
        execution_units = merge_contexts_into_units(tab.contexts)
        executions = []
        offset = 0
        for execution_index, unit in enumerate(execution_units):
            executions.append(
                Execution(
                    unit=unit,
                    context_offset=offset,
                    execution_name=execution_name(
                        bundle.lang_config, tab_index, execution_index
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
        result = _single_execution(bundle, executions, remaining)

        if result in (
            Status.TIME_LIMIT_EXCEEDED,
            Status.MEMORY_LIMIT_EXCEEDED,
            Status.OUTPUT_LIMIT_EXCEEDED,
        ):
            terminate(bundle, collector, result)
            return
        collector.add(CloseTab(), tab_index)

    collector.add(CloseJudgement())


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
    dependency_paths = bundle.lang_config.path_to_dependencies()
    copy_from_paths_to_path(dependency_paths, dependencies, common_dir)

    # Copy the submission file.
    submission = submission_file(bundle.lang_config)
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
        assert tab.contexts
        execution_units = merge_contexts_into_units(tab.contexts)
        for unit_i, unit in enumerate(execution_units):
            exec_name = execution_name(bundle.lang_config, tab_i, unit_i)
            _logger.debug(f"Generating file for execution {exec_name}")
            generated, evaluators = generate_execution(
                bundle=bundle,
                destination=common_dir,
                execution_unit=unit,
                execution_name=exec_name,
            )
            # Copy functions to the directory.
            for evaluator in evaluators:
                source = Path(bundle.config.resources) / evaluator
                _logger.debug("Copying oracle from %s to %s", source, common_dir)
                shutil.copy2(source, common_dir)
            dependencies.extend(evaluators)
            dependencies.append(generated)
            execution_names.append(exec_name)

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
        execution.collector.add(StartContext(description=context.description))

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

        execution.collector.add(CloseContext(), index)
        if continue_ in (Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED):
            return continue_
    return None


def _copy_workdir_source_files(bundle: Bundle, common_dir: Path) -> List[str]:
    """
    Copy additional source files from the workdir to the common dir

    :param bundle: Bundle information of the test suite
    :param common_dir: The directory of the other files
    """
    source_files = []

    def recursive_copy(src: Path, dst: Path):
        for origin in src.iterdir():
            file = origin.name.lower()
            if origin.is_file() and bundle.lang_config.is_source_file(origin):
                source_files.append(str(dst / origin.name))
                _logger.debug("Copying %s to %s", origin, dst)
                shutil.copy2(origin, dst)
            elif (
                origin.is_dir()
                and not file.startswith(EXECUTION_PREFIX)
                and file != "common"
            ):
                _logger.debug("Iterate subdir %s", dst / file)
                shutil.copytree(origin, dst / file)

    recursive_copy(bundle.config.workdir, common_dir)

    return source_files
