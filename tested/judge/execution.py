import itertools
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
from ..testplan import ExecutionMode, Run
from tested.internal_timings import new_stage, end_stage

_logger = logging.getLogger(__name__)


@dataclass
class RunTestcaseResult(BaseExecutionResult):
    """
    The result of a run testcase execution.

    All output streams are divided per testcase, in the same order as the
    context that was used to execute_module the test. E.g. the string at
    position 0 in stdout is the result of executing the testcase at position 0 in
    the context.
    """

    exception: str


@dataclass
class TestcaseResult(BaseExecutionResult):
    """
    The result of a testcase execution.

    All output streams are divided per testcase, in the same order as the
    context that was used to execute_module the test. E.g. the string at position
    0 in stdout is the result of executing the testcase at position 0 in the
    context.
    """

    separator: str
    results: str
    exceptions: str


@dataclass
class ExecutionResult(BaseExecutionResult):
    """
    The result of a context_testcase testcase execution.

    All output streams are divided per testcase, in the same order as the
    context that was used to execute_module the test. E.g. the string at position
    0 in stdout is the result of executing the testcase at position 0 in the
    context.
    """

    context_separator: str
    separator: str
    results: str
    exceptions: str

    def to_context_execution_results(
        self,
    ) -> Tuple[List[TestcaseResult], RunTestcaseResult]:
        results = self.results.split(self.context_separator)[1:]
        exceptions = self.exceptions.split(self.context_separator)[1:]
        stderr = self.stderr.split(self.context_separator)[1:]
        stdout = self.stdout.split(self.context_separator)[1:]

        size = max(len(results), len(exceptions), len(stderr), len(stdout))

        if size == 0:
            return [], RunTestcaseResult(
                exit=self.exit,
                exception="",
                stdout="",
                stderr="",
                timeout=self.timeout,
                memory=self.memory,
            )
        run_testcase = RunTestcaseResult(
            exit=self.exit,
            exception=(exceptions or [""])[0] or "",
            stdout=(stdout or [""])[0] or "",
            stderr=(stderr or [""])[0] or "",
            timeout=self.timeout and size <= 1,
            memory=self.memory and size <= 1,
        )

        size = size - 1

        context_execution_results = []
        for index, (r, e, err, out) in enumerate(
            itertools.zip_longest(results[1:], exceptions[1:], stderr[1:], stdout[1:]),
            start=1,
        ):
            context_execution_results.append(
                TestcaseResult(
                    separator=self.separator,
                    exit=self.exit,
                    results=r or "",
                    exceptions=e or "",
                    stdout=out or "",
                    stderr=err or "",
                    timeout=self.timeout and size == index,
                    memory=self.memory and size == index,
                )
            )

        return context_execution_results, run_testcase


class Execution(NamedTuple):
    """
    Arguments used to execute_module a single execution derived from the testplan.
    """

    run: Run
    context_offset: int
    execution_name: str
    execution_index: int
    mode: ExecutionMode
    common_directory: Path
    files: Union[List[str], Callable[[Path, str], bool]]
    precompilation_result: Optional[Tuple[List[Message], Status]]
    collector: OutputManager


def filter_files(files: Union[List[str], FileFilter], directory: Path) -> List[str]:
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
    argument: Optional[str] = None,
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
        arguments=[argument] if argument else [],
    )
    _logger.debug("Executing %s in directory %s", command, working_directory)

    result = run_command(working_directory, remaining, command, stdin)

    assert result is not None
    return result


def copy_workdir_files(bundle: Bundle, context_dir: Path):
    prefix = bundle.lang_config.execution_prefix()
    for origin in bundle.config.workdir.iterdir():
        file = origin.name.lower()
        if origin.is_file():
            _logger.debug("Copying %s to %s", origin, context_dir)
            shutil.copy2(origin, context_dir)
        elif origin.is_dir() and not file.startswith(prefix) and file != "common":
            _logger.debug("Copying %s to %s", origin, context_dir)
            shutil.copytree(origin, context_dir / file)


def execute_execution(
    bundle: Bundle, args: Execution, max_time: float
) -> Tuple[Optional[ExecutionResult], List[Message], Status, Path]:
    """
    Execute an execution.
    """
    lang_config = bundle.lang_config
    start = time.perf_counter()

    # Create a working directory for the execution.
    execution_dir = Path(bundle.config.workdir, args.execution_name)
    execution_dir.mkdir()

    _logger.info(
        "Executing execution %s in path %s", args.execution_name, execution_dir
    )

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
        # Fix weird OSError when copying th Haskell Selector executable
        if file.startswith(lang_config.selector_name()):
            os.link(origin, execution_dir / file)
        else:
            # noinspection PyTypeChecker
            shutil.copy2(origin, execution_dir)

    # If needed, do a compilation.
    if args.mode == ExecutionMode.INDIVIDUAL:
        new_stage("compilation.individual", True)
        _logger.info("Compiling context %s in INDIVIDUAL mode...", args.execution_name)
        remaining = max_time - (time.perf_counter() - start)
        result, files = run_compilation(bundle, execution_dir, dependencies, remaining)

        # A new compilation means a new file filtering
        files = filter_files(files, execution_dir)

        # Process compilation results.
        messages, status, annotations = process_compile_results(
            bundle.plan.namespace, lang_config, result
        )

        for annotation in annotations:
            args.collector.add(annotation)

        if status != Status.CORRECT:
            _logger.debug("Compilation of individual context failed.")
            _logger.debug("Aborting executing of this context.")
            return None, messages, status, execution_dir

        _logger.debug("Executing context %s in INDIVIDUAL mode...", args.execution_name)

        executable, messages, status, annotations = lang_config.find_main_file(
            files, args.execution_name
        )

        for annotation in annotations:
            args.collector.add(annotation)

        if status != Status.CORRECT:
            return None, messages, status, execution_dir

        files.remove(executable)
        stdin = args.run.get_stdin(bundle.config.resources)
        argument = None
    else:
        new_stage("compilation.batch.done", True)
        result, files = None, list(dependencies)
        if args.precompilation_result:
            _logger.debug("Substituting precompilation results.")
            messages, status = args.precompilation_result
        else:
            _logger.debug("No precompilation results found, using default.")
            messages, status = [], Status.CORRECT

        _logger.info(
            "Executing context %s in PRECOMPILATION mode...", args.execution_name
        )

        if lang_config.needs_selector():
            _logger.debug("Selector is needed, using it.")

            selector_name = lang_config.selector_name()

            executable, messages, status, annotations = lang_config.find_main_file(
                files, selector_name
            )

            for annotation in annotations:
                args.collector.add(annotation)

            if status != Status.CORRECT:
                return None, messages, status, execution_dir

            files.remove(executable)
            stdin = args.run.get_stdin(bundle.config.resources)
            argument = args.execution_name
        else:
            _logger.debug("Selector is not needed, using individual execution.")

            executable, messages, status, annotations = lang_config.find_main_file(
                files, args.execution_name
            )

            for annotation in annotations:
                args.collector.add(annotation)

            if status != Status.CORRECT:
                return None, messages, status, execution_dir

            files.remove(executable)
            stdin = args.run.get_stdin(bundle.config.resources)
            argument = None

    remaining = max_time - (time.perf_counter() - start)

    new_stage("run.testcode", True)
    # Do the execution.
    base_result = execute_file(
        bundle,
        executable_name=executable,
        working_directory=execution_dir,
        stdin=stdin,
        argument=argument,
        remaining=remaining,
    )

    new_stage("prepare.results", True)
    # Cleanup stderr
    msgs, annotations, base_result.stderr = lang_config.stderr(
        bundle, base_result.stderr
    )
    for annotation in annotations:
        args.collector.add(annotation)
    messages.extend(msgs)
    # Cleanup stdout
    msgs, annotation, base_result.stdout = lang_config.stdout(
        bundle, base_result.stdout
    )
    for annotation in annotations:
        args.collector.add(annotation)
    messages.extend(msgs)

    identifier = f"--{bundle.secret}-- SEP"
    context_identifier = f"--{bundle.context_separator_secret}-- SEP"

    value_file_path = value_file(bundle, execution_dir)
    try:
        with open(value_file_path, "r") as f:
            values = f.read()
    except FileNotFoundError:
        _logger.warning("Value file not found, looked in %s", value_file_path)
        values = ""

    exception_file_path = exception_file(bundle, execution_dir)
    try:
        # noinspection PyTypeChecker
        with open(exception_file_path, "r") as f:
            exceptions = f.read()
    except FileNotFoundError:
        _logger.warning("Exception file not found, looked in %s", exception_file_path)
        exceptions = ""

    result = ExecutionResult(
        stdout=base_result.stdout,
        stderr=base_result.stderr,
        exit=base_result.exit,
        context_separator=context_identifier,
        separator=identifier,
        results=values,
        exceptions=exceptions,
        timeout=base_result.timeout,
        memory=base_result.memory,
    )

    return result, messages, status, execution_dir
