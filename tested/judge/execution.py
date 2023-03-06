import itertools
import logging
import shutil
from pathlib import Path
from typing import List, Optional, Tuple, Callable, Union

import time
from dataclasses import dataclass

from utils import safe_del
from .collector import OutputManager
from .compilation import run_compilation, process_compile_results
from .utils import BaseExecutionResult, run_command
from ..configs import Bundle
from ..dodona import Message, Status
from ..languages.config import FileFilter, Config
from ..languages.generator import value_file, exception_file
from ..testplan import ExecutionMode, Context, EmptyChannel
from tested.internal_timings import new_stage

_logger = logging.getLogger(__name__)


@dataclass
class ContextResult(BaseExecutionResult):
    """
    The results of executing a context.

    All output streams are divided by the testcase separator, in the same order
    as the testcases in the context in the test suite. For example, the string
    at position 0 of the split output is the output for the first testcase.
    """

    separator: str
    results: str
    exceptions: str


@dataclass
class ExecutionResult(BaseExecutionResult):
    """
    The results of executing an execution unit.

    All the output is divided by the context separator, in the same order as
    the contexts from the test suite. For example, the string at position 0 of
    the split output is the output for the first context.
    """

    context_separator: str
    testcase_separator: str
    results: str
    exceptions: str

    def to_context_results(
        self,
    ) -> List[ContextResult]:

        results = self.results.split(self.context_separator)
        exceptions = self.exceptions.split(self.context_separator)
        stderr = self.stderr.split(self.context_separator)
        stdout = self.stdout.split(self.context_separator)
        size = max(len(results), len(exceptions), len(stderr), len(stdout))

        # Since the context separator is first, we should have one that is empty.
        # We only remove it if it is in fact empty, otherwise ignore it.
        safe_del(stdout, 0, lambda e: e == "")
        safe_del(stderr, 0, lambda e: e == "")
        safe_del(exceptions, 0, lambda e: e == "")
        safe_del(results, 0, lambda e: e == "")

        if size == 0:
            return [
                ContextResult(
                    exit=self.exit,
                    exceptions="",
                    stdout="",
                    stderr="",
                    timeout=self.timeout,
                    memory=self.memory,
                    separator=self.testcase_separator,
                    results="",
                )
            ]

        context_execution_results = []
        for index, (r, e, err, out) in enumerate(
            itertools.zip_longest(results, exceptions, stderr, stdout)
        ):
            context_execution_results.append(
                ContextResult(
                    separator=self.testcase_separator,
                    exit=self.exit,
                    results=r or "",
                    exceptions=e or "",
                    stdout=out or "",
                    stderr=err or "",
                    timeout=self.timeout and size == index,
                    memory=self.memory and size == index,
                )
            )

        return context_execution_results


@dataclass
class ExecutionUnit:
    """
    Combines a set of contexts that will be executed togheter.
    """

    contexts: List[Context]

    def get_stdin(self, resources: Path) -> str:
        return "\n".join(c.get_stdin(resources) or "" for c in self.contexts)

    def has_main_testcase(self) -> bool:
        return self.contexts[0].has_main_testcase()

    def has_exit_testcase(self) -> bool:
        return self.contexts[-1].has_exit_testcase()


@dataclass
class Execution:
    """
    Contains an execution unit and various metadata.
    """

    unit: ExecutionUnit
    context_offset: int
    execution_name: str
    execution_index: int
    mode: ExecutionMode
    common_directory: Path
    files: Union[List[str], Callable[[Path, str], bool]]
    precompilation_result: Optional[Tuple[List[Message], Status]]
    collector: OutputManager


def filter_files(files: Union[List[str], FileFilter], directory: Path) -> List[Path]:
    if callable(files):
        return list(
            x.relative_to(directory) for x in filter(files, directory.rglob("*"))
        )
    else:
        return [Path(file) for file in files]


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

    _logger.info("Executing %s in path %s", args.execution_name, execution_dir)

    new_stage("dependencies.copy", True)
    # Filter dependencies of the global compilation results.
    dependencies = filter_files(args.files, args.common_directory)
    dependencies = bundle.lang_config.filter_dependencies(
        bundle, dependencies, args.execution_name
    )
    _logger.debug("Dependencies are %s", dependencies)
    copy_workdir_files(bundle, execution_dir)

    # Copy files from the common directory to the context directory.
    for file in dependencies:
        origin = args.common_directory / file
        destination = execution_dir / file
        # Ensure we preserve subdirectories.
        destination.parent.mkdir(parents=True, exist_ok=True)
        _logger.debug("Copying %s to %s", origin, destination)
        if origin == destination:
            continue  # Don't copy the file to itself
        shutil.copy2(origin, destination)

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
            files, args.execution_name, messages
        )

        for annotation in annotations:
            args.collector.add(annotation)

        if status != Status.CORRECT:
            return None, messages, status, execution_dir

        files.remove(executable)
        stdin = args.unit.get_stdin(bundle.config.resources)
        argument = None
    else:
        new_stage("compilation.batch.done", True)
        result, files = None, list(dependencies)
        if args.precompilation_result:
            _logger.debug("Substituting precompilation results.")
            messages, _ = args.precompilation_result
        else:
            _logger.debug("No precompilation results found, using default.")
            messages, _ = [], Status.CORRECT

        _logger.info(
            "Executing context %s in PRECOMPILATION mode...", args.execution_name
        )

        if lang_config.needs_selector():
            _logger.debug("Selector is needed, using it.")

            selector_name = lang_config.selector_name()

            executable, messages, status, annotations = lang_config.find_main_file(
                files, selector_name, messages
            )

            _logger.debug(f"Found main file: {executable}")

            for annotation in annotations:
                args.collector.add(annotation)

            if status != Status.CORRECT:
                return None, messages, status, execution_dir

            files.remove(executable)
            stdin = args.unit.get_stdin(bundle.config.resources)
            argument = args.execution_name
        else:
            _logger.debug("Selector is not needed, using individual execution.")

            executable, messages, status, annotations = lang_config.find_main_file(
                files, args.execution_name, messages
            )

            for annotation in annotations:
                args.collector.add(annotation)

            if status != Status.CORRECT:
                return None, messages, status, execution_dir

            files.remove(executable)
            stdin = args.unit.get_stdin(bundle.config.resources)
            argument = None

    remaining = max_time - (time.perf_counter() - start)

    new_stage("run.testcode", True)
    # Do the execution.
    base_result = execute_file(
        bundle,
        executable_name=executable.name,
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

    testcase_identifier = f"--{bundle.secret}-- SEP"
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
        testcase_separator=testcase_identifier,
        results=values,
        exceptions=exceptions,
        timeout=base_result.timeout,
        memory=base_result.memory,
    )

    return result, messages, status, execution_dir


def merge_contexts_into_units(contexts: List[Context]) -> List[ExecutionUnit]:
    """
    Merge contexts into as little execution units as possible.

    :param contexts:
    :return:
    """
    # return [ExecutionUnit(contexts=[c]) for c in contexts]

    units = []
    current_unit = []

    for context in contexts:

        # If we get stdin, start a new execution unit.
        if (
            context.has_main_testcase()
            and context.testcases[0].input.stdin != EmptyChannel.NONE
        ):
            if current_unit:
                units.append(ExecutionUnit(contexts=current_unit))
            current_unit = []

        current_unit.append(context)

        if context.has_exit_testcase():
            units.append(ExecutionUnit(contexts=current_unit))
            current_unit = []

    if current_unit:
        units.append(ExecutionUnit(contexts=current_unit))

    return units
