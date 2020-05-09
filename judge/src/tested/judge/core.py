import concurrent
import logging
import shutil
from concurrent.futures.thread import ThreadPoolExecutor
from pathlib import Path
from typing import Tuple, List

import time

from .collector import OutputManager
from .compilation import run_compilation, process_compile_results
from .evaluation import evaluate_results
from .execution import ContextExecution, execute_context
from .linter import run_linter
from .utils import copy_from_paths_to_path
from ..configs import Bundle
from ..dodona import *
from ..features import is_supported
from ..languages.generator import generate_context, generate_selector
from ..languages.templates import path_to_templates
from ..testplan import ExecutionMode

_logger = logging.getLogger(__name__)


def judge(bundle: Bundle):
    """
    Evaluate a solution for an exercise. Execute the tests present in the
    testplan. The result (the judgment) is sent to stdout, so Dodona can pick it
    up.

    :param bundle: The configuration bundle.
    """
    # Begin by checking if the given testplan is executable in this language.
    _logger.info("Checking supported features...")
    if not is_supported(bundle):
        report_update(bundle.out, StartJudgment())
        report_update(bundle.out, CloseJudgment(
            accepted=False,
            status=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human=f"Deze oefening kan niet opgelost worden in deze "
                      f"programmeertaal: {bundle.config.programming_language}"
            )
        ))
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
        # Compile all code in one go.
        _logger.info("Running precompilation step...")
        remaining = max_time - (time.perf_counter() - start)
        result, compilation_files = run_compilation(bundle, common_dir, files,
                                                    remaining)

        messages, status, annotations = process_compile_results(
            bundle.lang_config,
            result
        )

        # If there is no result, there was no compilation.
        if not result:
            precompilation_result = None
        else:
            # Handle timout if necessary.
            if result.timeout:
                # Show in separate tab.
                index = len(bundle.plan.tabs) + 1
                if messages:
                    collector.prepare_tab(StartTab("Compilatie"), index)
                for message in messages:
                    collector.add(AppendMessage(message=message))
                for annotation in annotations:
                    collector.add(annotation)
                if messages:
                    collector.prepare_tab(CloseTab(), index)
                collector.terminate(Status.TIME_LIMIT_EXCEEDED)
                return
            if result.memory:
                index = len(bundle.plan.tabs) + 1
                # Show in separate tab.
                if messages:
                    collector.prepare_tab(StartTab("Compilatie"), index)
                for message in messages:
                    collector.add(AppendMessage(message=message))
                for annotation in annotations:
                    collector.add(annotation)
                if messages:
                    collector.prepare_tab(CloseTab(), index)
                collector.terminate(Status.MEMORY_LIMIT_EXCEEDED)
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
            else:
                files = compilation_files
                # Report messages.
                if messages:
                    collector.add_tab(StartTab("Compilatie"), -1)
                for message in messages:
                    collector.add(AppendMessage(message=message))
                for annotation in annotations:
                    collector.add(annotation)
                if messages:
                    collector.add_tab(CloseTab(), -1)

                if status != Status.CORRECT:
                    collector.terminate(StatusMessage(
                        enum=status,
                        human="Ongeldige broncode"
                    ))
                    _logger.info("Compilation error without fallback")
                    return  # Compilation error occurred, useless to continue.
    else:
        precompilation_result = None

    _logger.info("Starting judgement...")
    parallel = bundle.config.options.parallel

    # Create a list of contexts we want to execute.
    for tab_index, tab in enumerate(bundle.plan.tabs):
        collector.add_tab(StartTab(title=tab.name), tab_index)
        executions = []
        for context_index, context in enumerate(tab.contexts):
            execution = ContextExecution(
                context=context,
                context_name=bundle.lang_config.context_name(
                    tab_number=tab_index,
                    context_number=context_index
                ),
                context_index=context_index,
                mode=mode,
                common_directory=common_dir,
                files=files,
                precompilation_result=precompilation_result,
                collector=collector
            )
            executions.append(execution)

        remaining = max_time - (time.perf_counter() - start)
        if parallel:
            result = _parallel_execution(bundle, executions, remaining)
        else:
            result = _single_execution(bundle, executions, remaining)

        if result in (Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED):
            assert not collector.collected
            collector.terminate(result)
            return
        collector.add_tab(CloseTab(), tab_index)
    collector.add(CloseJudgment())
    collector.clean_finish()


def _single_execution(bundle: Bundle,
                      items: List[ContextExecution],
                      max_time: float) -> Optional[Status]:
    """
    Process items in a non-threaded way.

    :param bundle: The configuration bundle.
    :param items: The contexts to execute.
    :param max_time: The max amount of time.
    """
    start = time.perf_counter()
    for execution in items:
        execution.collector.add_context(
            StartContext(description=execution.context.description),
            execution.context_index
        )

        remaining = max_time - (time.perf_counter() - start)
        execution_result, m, s, p = execute_context(bundle, execution, remaining)
        continue_ = evaluate_results(bundle, context=execution.context,
                                     exec_results=execution_result,
                                     compiler_results=(m, s), context_dir=p,
                                     collector=execution.collector)
        if continue_ in (Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED):
            return continue_
        else:
            execution.collector.add_context(CloseContext(), execution.context_index)

def _parallel_execution(bundle: Bundle,
                        items: List[ContextExecution],
                        max_time: float) -> Optional[Status]:
    """
    Execute a list of contexts in parallel.

    :param bundle: The configuration bundle.
    :param items: The contexts to execute.
    :param max_time: The max amount of time.
    """
    start = time.perf_counter()  # Accessed from threads.

    def threaded_execution(execution: ContextExecution):
        """The function executed in parallel."""
        remainder = max_time - (time.perf_counter() - start)
        execution_result, m, s, p = execute_context(bundle, execution, remainder)

        def evaluation_function(eval_remainder):
            execution.collector.add_context(
                StartContext(description=execution.context.description),
                execution.context_index
            )
            continue_ = evaluate_results(bundle, context=execution.context,
                                         exec_results=execution_result,
                                         compiler_results=(m, s), context_dir=p,
                                         collector=execution.collector)
            execution.collector.add_context(CloseContext(), execution.context_index)
            return continue_

        return evaluation_function

    with ThreadPoolExecutor(max_workers=4) as executor:
        remaining = max_time - (time.perf_counter() - start)
        results = executor.map(threaded_execution, items, timeout=remaining)
        try:
            for eval_function in list(results):
                remaining = max_time - (time.perf_counter() - start)
                if (status := eval_function(remaining)) in (
                        Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED):
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
    bundle.lang_config.solution(solution_path, bundle)

    # The names of the contexts in the testplan.
    context_names = []
    # Generate the files for each context.
    for tab_i, tab in enumerate(bundle.plan.tabs):
        for context_i, context in enumerate(tab.contexts):
            context_name = bundle.lang_config.context_name(tab_i, context_i)
            _logger.debug(f"Generating file for context {context_name}")
            generated, evaluators = generate_context(
                bundle=bundle,
                destination=common_dir,
                context=context,
                context_name=context_name
            )
            # Copy evaluators to the directory.
            for evaluator in evaluators:
                source = Path(bundle.config.resources) / evaluator
                _logger.debug("Copying evaluator from %s to %s",
                              source, common_dir)
                shutil.copy2(source, common_dir)
            dependencies.extend(evaluators)
            dependencies.append(generated)
            context_names.append(context_name)

    if mode == ExecutionMode.PRECOMPILATION \
            and bundle.lang_config.needs_selector():
        _logger.debug("Generating selector for PRECOMPILATION mode.")
        generated = generate_selector(bundle, common_dir, context_names)
    else:
        generated = None
    return common_dir, dependencies, generated
