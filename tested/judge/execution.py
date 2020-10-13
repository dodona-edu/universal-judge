import logging
import os
import shutil
from pathlib import Path
from typing import List, Optional, Tuple, NamedTuple, Callable, Union

import time
from pydantic.dataclasses import dataclass

from .collector import OutputManager
from .compilation import run_compilation, process_compile_results
from .utils import BaseExecutionResult, run_command
from ..configs import Bundle
from ..dodona import Message, Status
from ..languages.config import FileFilter, Config
from ..languages.generator import value_file, exception_file
from ..testplan import Context, ExecutionMode

_logger = logging.getLogger(__name__)


@dataclass
class ExecutionResult(BaseExecutionResult):
    """
    The result of a context_testcase testcase execution.

    All output streams are divided per testcase, in the same order as the
    context that was used to execute_module the test. E.g. the string at position
    0 in
    stdout is the result of executing the testcase at position 0 in the context.
    """
    separator: str
    results: str
    exceptions: str


class ContextExecution(NamedTuple):
    """
    Arguments used to execute_module a single context of the testplan.
    """
    context: Context  # The context object.
    context_name: str  # Name of the context instance (used in code)
    context_index: int  # Index of the context within the tab.
    mode: ExecutionMode
    common_directory: Path
    files: Union[List[str], Callable[[Path, str], bool]]
    precompilation_result: Optional[Tuple[List[Message], Status]]
    collector: OutputManager


def filter_files(files: Union[List[str], FileFilter],
                 directory: Path) -> List[str]:
    if callable(files):
        return list(x.name for x in filter(files, directory.glob("*")))
    else:
        return files


def execute_file(
        bundle: Bundle,
        executable_name: str,
        working_directory: Path,
        remaining: Optional[float],
        stdin: Optional[str] = None,
        argument: Optional[str] = None
) -> BaseExecutionResult:
    """
    Execute a file.

    Note that this method must be thread-safe.

    :param bundle: The configuration bundle.
    :param working_directory: The working directory, in which the execution must
                              take place.
    :param argument: Argument for the executable, optional.
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
        arguments=[argument] if argument else []
    )
    _logger.debug("Executing %s in directory %s", command, working_directory)

    result = run_command(working_directory, remaining, command, stdin)
    assert result is not None
    return result


def execute_context(bundle: Bundle, args: ContextExecution, max_time: float) \
        -> Tuple[Optional[ExecutionResult], List[Message], Status, Path]:
    """
    Execute a context.
    """
    lang_config = bundle.lang_config
    start = time.perf_counter()

    # Create a working directory for the context.
    context_dir = Path(
        bundle.config.workdir,
        args.context_name
    )
    context_dir.mkdir()

    _logger.info("Executing context %s in path %s", args.context_name, context_dir)

    # Filter dependencies of the global compilation results.
    dependencies = filter_files(args.files, args.common_directory)
    dependencies = bundle.lang_config.filter_dependencies(
        bundle, dependencies, args.context_name
    )

    # Copy files from the common directory to the context directory.
    for file in dependencies:
        origin = args.common_directory / file
        _logger.debug("Copying %s to %s", origin, context_dir)
        # Fix weird OSError when copying th Haskell Selector executable
        if file.startswith(lang_config.selector_name()):
            os.link(origin, context_dir / file)
        else:
            # noinspection PyTypeChecker
            shutil.copy2(origin, context_dir)

    # If needed, do a compilation.
    if args.mode == ExecutionMode.INDIVIDUAL:
        _logger.info("Compiling context %s in INDIVIDUAL mode...",
                     args.context_name)
        remaining = max_time - (time.perf_counter() - start)
        result, files = run_compilation(bundle, context_dir, dependencies,
                                        remaining)

        # A new compilation means a new file filtering
        files = filter_files(files, context_dir)

        # Process compilation results.
        messages, status, annotations = process_compile_results(
            lang_config,
            result
        )

        for annotation in annotations:
            args.collector.add(annotation)

        if status != Status.CORRECT:
            _logger.debug("Compilation of individual context failed.")
            _logger.debug("Aborting executing of this context.")
            return None, messages, status, context_dir

        _logger.debug("Executing context %s in INDIVIDUAL mode...",
                      args.context_name)

        executable, messages, status, annotations = \
            lang_config.find_main_file(files, args.context_name)

        for annotation in annotations:
            args.collector.add(annotation)

        if status != Status.CORRECT:
            return None, messages, status, context_dir

        files.remove(executable)
        stdin = args.context.get_stdin(bundle.config.resources)
        argument = None
    else:
        result, files = None, list(dependencies)
        if args.precompilation_result:
            _logger.debug("Substituting precompilation results.")
            messages, status = args.precompilation_result
        else:
            _logger.debug("No precompilation results found, using default.")
            messages, status = [], Status.CORRECT

        _logger.info("Executing context %s in PRECOMPILATION mode...",
                     args.context_name)

        if lang_config.needs_selector():
            _logger.debug("Selector is needed, using it.")

            selector_name = lang_config.selector_name()

            executable, messages, status, annotations = \
                lang_config.find_main_file(files, selector_name)

            for annotation in annotations:
                args.collector.add(annotation)

            if status != Status.CORRECT:
                return None, messages, status, context_dir

            files.remove(executable)
            stdin = args.context.get_stdin(bundle.config.resources)
            argument = args.context_name
        else:
            _logger.debug("Selector is not needed, using individual execution.")

            executable, messages, status, annotations = \
                lang_config.find_main_file(files, args.context_name)

            for annotation in annotations:
                args.collector.add(annotation)

            if status != Status.CORRECT:
                return None, messages, status, context_dir

            files.remove(executable)
            stdin = args.context.get_stdin(bundle.config.resources)
            argument = None

    remaining = max_time - (time.perf_counter() - start)
    # Do the execution.
    base_result = execute_file(
        bundle,
        executable_name=executable,
        working_directory=context_dir,
        stdin=stdin,
        argument=argument,
        remaining=remaining
    )

    identifier = f"--{bundle.secret}-- SEP"

    value_file_path = value_file(bundle, context_dir)
    try:
        with open(value_file_path, "r") as f:
            values = f.read()
    except FileNotFoundError:
        _logger.warning("Value file not found, looked in %s", value_file_path)
        values = ""

    exception_file_path = exception_file(bundle, context_dir)
    try:
        # noinspection PyTypeChecker
        with open(exception_file_path, "r") as f:
            exceptions = f.read()
    except FileNotFoundError:
        _logger.warning("Exception file not found, looked in %s",
                        exception_file_path)
        exceptions = ""

    result = ExecutionResult(
        stdout=base_result.stdout,
        stderr=base_result.stderr,
        exit=base_result.exit,
        separator=identifier,
        results=values,
        exceptions=exceptions,
        timeout=base_result.timeout,
        memory=base_result.memory
    )

    return result, messages, status, context_dir
