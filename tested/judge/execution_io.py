import concurrent
import logging
import os
import shutil
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import NamedTuple, Callable, List, Union, Optional, Tuple

from tested.configs import Bundle
from tested.dodona import Status, Message, StartContext, CloseContext
from tested.internal_timings import new_stage, end_stage
from tested.internationalization import get_i18n_string
from tested.judge.collector import OutputManager
from tested.judge.evaluation import evaluate_run_results
from tested.judge.execution import filter_files, \
    copy_workdir_files, RunTestcaseResult
from tested.judge.utils import BaseExecutionResult, run_command
from tested.languages.config import Config
from tested.testplan import RunTestcase

_logger = logging.getLogger(__name__)


class ExecutionIO(NamedTuple):
    """
    Arguments used to execute_module a single io execution derived from the
    testplan.
    """
    run: RunTestcase
    context_offset: int
    execution_name: str
    execution_index: int
    tab_index: int
    common_directory: Path
    files: Union[List[str], Callable[[Path, str], bool]]
    collector: OutputManager


def single_io_execution(bundle: Bundle,
                        items: List[ExecutionIO],
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
        execution_result, m, s, p = execute_io_execution(bundle, execution,
                                                         remaining)

        new_stage("evaluate.results")
        status = _process_results(bundle, execution, execution_result, m, s, p)
        end_stage("evaluate.results")

        if status:
            return status


def parallel_io_execution(bundle: Bundle,
                          items: List[ExecutionIO],
                          max_time: float) -> Optional[Status]:
    """
    Execute a list of contexts in parallel.

    :param bundle: The configuration bundle.
    :param items: The contexts to execute.
    :param max_time: The max amount of time.
    """
    start = time.perf_counter()  # Accessed from threads.

    def threaded_execution(execution: ExecutionIO):
        """The function executed in parallel."""
        remainder = max_time - (time.perf_counter() - start)
        new_stage("run.execution")
        execution_result, m, s, p = execute_io_execution(bundle, execution,
                                                         remainder)
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


def execute_io_execution(bundle: Bundle, args: ExecutionIO, max_time: float) \
        -> Tuple[Optional[RunTestcaseResult], List[Message], Status, Path]:
    """
    Execute an execution.
    """
    lang_config = bundle.lang_config
    start = time.perf_counter()

    # Create a working directory for the execution.
    execution_dir = Path(
        bundle.config.workdir,
        lang_config.execution_name(0, args.execution_index)
    )
    execution_dir.mkdir()

    _logger.info("Executing execution %s in path %s", args.execution_name,
                 execution_dir)

    new_stage("dependencies.copy", True)
    # Filter dependencies of the global compilation results.
    dependencies = filter_files(args.files, args.common_directory)
    dependencies = bundle.lang_config.filter_dependencies(
        bundle, dependencies, args.execution_name
    )
    copy_workdir_files(bundle, execution_dir)

    # Copy files from the common directory to the context directory.
    for file in dependencies:
        origin = args.common_directory / file
        _logger.debug("Copying %s to %s", origin, execution_dir)
        # Fix weird OSError when copying the submission executable
        if file.startswith(lang_config.submission_name(bundle.plan)):
            os.link(origin, execution_dir / file)
        else:
            # noinspection PyTypeChecker
            shutil.copy2(origin, execution_dir)

    # Detect execution file
    executable, messages, status, annotations = \
        lang_config.find_main_file(dependencies, args.execution_name)

    for annotation in annotations:
        args.collector.add(annotation)

    if status != Status.CORRECT:
        return None, messages, status, execution_dir

    remaining = max_time - (time.perf_counter() - start)

    new_stage("run.testcode", True)
    # Do the execution.
    base_result = execute_io_file(
        bundle,
        executable_name=executable,
        working_directory=execution_dir,
        stdin=args.run.input.get_as_string(execution_dir),
        arguments=args.run.input.arguments,
        remaining=remaining
    )

    new_stage("prepare.results", True)
    # Cleanup stderr
    msgs, annotations, base_result.stderr = lang_config.stderr(bundle,
                                                               base_result.stderr)
    for annotation in annotations:
        args.collector.add(annotation)
    messages.extend(msgs)
    # Cleanup stdout
    msgs, annotation, base_result.stdout = lang_config.stdout(bundle,
                                                              base_result.stdout)
    for annotation in annotations:
        args.collector.add(annotation)
    messages.extend(msgs)

    result = RunTestcaseResult(stdout=base_result.stdout,
                               stderr=base_result.stderr,
                               exit=base_result.exit,
                               timeout=base_result.timeout,
                               memory=base_result.memory,
                               exception="")

    return result, messages, status, execution_dir


def execute_io_file(
        bundle: Bundle,
        executable_name: str,
        working_directory: Path,
        remaining: Optional[float],
        stdin: Optional[str] = None,
        arguments: Optional[List[str]] = None
) -> BaseExecutionResult:
    """
    Execute a file.

    Note that this method must be thread-safe.

    :param arguments: Arguments for the executable
    :param bundle: The configuration bundle.
    :param working_directory: The working directory, in which the execution must
                              take place.
    :param stdin: The stdin for the execution.
    :param executable_name: The executable that should be executed. This file
                            will not be present in the dependency list.
    :param remaining: The max amount of time.

    :return: The result of the execution.
    """
    _logger.info("Starting execution on file %s", executable_name)

    command = bundle.lang_config.execution(
        config=Config.from_bundle(bundle),
        cwd=working_directory,
        file=executable_name,
        arguments=arguments if arguments else []
    )
    _logger.debug("Executing %s in directory %s", command, working_directory)

    result = run_command(working_directory, remaining, command, stdin)

    assert result is not None
    return result


def _process_results(bundle: Bundle, execution: ExecutionIO,
                     run_testcase: Optional[RunTestcaseResult], m: List[Message],
                     s: Status, p: Path) -> Optional[Status]:
    offset = execution.context_offset
    execution.collector.add_context(
        StartContext(),
        offset
    )
    continue_ = evaluate_run_results(bundle, execution.run, run_testcase,
                                     (m, s), p, execution.collector, False)
    execution.collector.add_context(CloseContext(), offset)
    if execution.collector.is_full():
        return Status.OUTPUT_LIMIT_EXCEEDED
    if continue_ in (Status.TIME_LIMIT_EXCEEDED,
                     Status.MEMORY_LIMIT_EXCEEDED):
        return continue_
