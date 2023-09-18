import enum
import itertools
import logging
import shutil
from enum import Enum
from pathlib import Path
from typing import List, Optional, Tuple, Union, cast

from attrs import define

from tested.configs import Bundle
from tested.dodona import Message, Status
from tested.judge.compilation import process_compile_results, run_compilation
from tested.judge.planning import (
    CompilationResult,
    ExecutionPlan,
    PlannedContext,
    PlannedExecutionUnit,
)
from tested.judge.utils import BaseExecutionResult, run_command
from tested.languages.config import FileFilter
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    execution_name,
    selector_name,
)
from tested.languages.preparation import exception_file, value_file
from tested.testsuite import EmptyChannel, MainInput
from tested.utils import safe_del

_logger = logging.getLogger(__name__)


@define
class ContextResult(BaseExecutionResult):
    """
    The results of executing a context.

    All output streams are divided by the testcase separator, in the same order
    as the test cases in the context in the test suite. For example, the string
    at position 0 of the split output is the output for the first testcase.
    """

    separator: str
    results: str
    exceptions: str


@define
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

        # Since the context separator is first, we should have one that is empty.
        # We only remove it if it is in fact empty, otherwise ignore it.
        safe_del(stdout, 0, lambda e: e == "")
        safe_del(stderr, 0, lambda e: e == "")
        safe_del(exceptions, 0, lambda e: e == "")
        safe_del(results, 0, lambda e: e == "")

        size = max(len(results), len(exceptions), len(stderr), len(stdout))

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
                    timeout=self.timeout and index == size - 1,
                    memory=self.memory and index == size - 1,
                )
            )

        return context_execution_results


@define
class ExecutionUnit:
    """
    Contains an execution unit and various metadata.
    """

    planned: PlannedExecutionUnit
    files: Union[List[str], FileFilter]
    precompilation_result: Optional[Tuple[List[Message], Status]]


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

    command = bundle.language.execution(
        cwd=working_directory,
        file=executable_name,
        arguments=[argument] if argument else [],
    )
    _logger.debug("Executing %s in directory %s", command, working_directory)

    result = run_command(working_directory, remaining, command, stdin)

    assert result is not None
    return result


def copy_workdir_files(bundle: Bundle, context_dir: Path):
    for origin in bundle.config.workdir.iterdir():
        file = origin.name.lower()
        if origin.is_file():
            _logger.debug("Copying %s to %s", origin, context_dir)
            shutil.copy2(origin, context_dir)
        elif (
            origin.is_dir()
            and not file.startswith(EXECUTION_PREFIX)
            and file != "common"
        ):
            _logger.debug("Copying %s to %s", origin, context_dir)
            shutil.copytree(origin, context_dir / file)


def _get_contents_or_empty(file_path: Path) -> str:
    try:
        with open(file_path, "r") as f:
            return f.read()
    except FileNotFoundError:
        _logger.warning(f"File not found, looked in {file_path}")
        return ""


def set_up_unit(
    bundle: Bundle, plan: ExecutionPlan, which_unit: int
) -> tuple[Path, list[Path]]:
    unit = plan.units[which_unit]
    # Create a working directory for the execution.
    execution_dir = Path(bundle.config.workdir, unit.name)
    execution_dir.mkdir()

    _logger.info(f"Preparing {unit.name} in {execution_dir}")

    # Filter dependencies of the global compilation results.
    dependencies = filter_files(plan.files, plan.common_directory)
    dependencies = bundle.language.filter_dependencies(dependencies, unit.name)
    _logger.debug(f"Dependencies are {dependencies}")
    copy_workdir_files(bundle, execution_dir)

    # Copy files from the common directory to the context directory.
    for file in dependencies:
        origin = plan.common_directory / file
        destination = execution_dir / file
        # Ensure we preserve subdirectories.
        destination.parent.mkdir(parents=True, exist_ok=True)
        _logger.debug(f"Copying {origin} to {destination}")
        if origin == destination:
            continue  # Don't copy the file to itself
        shutil.copy2(origin, destination)

    return execution_dir, dependencies


def compile_unit(
    bundle: Bundle,
    plan: ExecutionPlan,
    which_unit: int,
    execution_dir: Path,
    dependencies: list[Path],
) -> tuple[CompilationResult, list[Path]]:
    unit = plan.units[which_unit]
    _logger.info(f"Compiling unit {unit.name}")
    remaining = plan.remaining_time()
    deps = [str(x) for x in dependencies]
    result, files = run_compilation(bundle, execution_dir, deps, remaining)

    # A new compilation means a new file filtering
    files = filter_files(files, execution_dir)

    # Process compilation results.
    processed_results = process_compile_results(bundle.language, result)
    return processed_results, files


def execute_unit(
    bundle: Bundle,
    unit: PlannedExecutionUnit,
    execution_dir: Path,
    dependencies: list[Path],
    remaining_time: float,
) -> tuple[ExecutionResult | None, Status]:
    """
    Execute a unit.

    This function assumes the files have been prepared (set_up_unit) and
    compilation has happened if needed.

    :param bundle: The bundle.
    :param unit: The unit to execute.
    :param execution_dir: The directory in which we execute.
    :param dependencies: The dependencies.
    :param remaining_time: The remaining time for this execution.
    """
    _logger.info(f"Executing unit {unit.name}")

    files = list(dependencies)  # A copy of the files.

    if bundle.language.needs_selector():
        main_file_name = selector_name(bundle.language)
        argument = unit.name
    else:
        main_file_name = unit.name
        argument = None

    executable, status = bundle.language.find_main_file(files, main_file_name)
    _logger.debug(f"Found main file: {executable}")

    if status != Status.CORRECT:
        return None, status

    files.remove(executable)
    stdin = unit.get_stdin(bundle.config.resources)

    # Do the execution.
    base_result = execute_file(
        bundle,
        executable_name=executable.name,
        working_directory=execution_dir,
        stdin=stdin,
        argument=argument,
        remaining=remaining_time,
    )

    testcase_identifier = f"--{bundle.testcase_separator_secret}-- SEP"
    context_identifier = f"--{bundle.context_separator_secret}-- SEP"

    values = _get_contents_or_empty(value_file(bundle, execution_dir))
    exceptions = _get_contents_or_empty(exception_file(bundle, execution_dir))

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

    return result, status


class PlanStrategy(Enum):
    OPTIMAL = enum.auto()
    TAB = enum.auto()
    CONTEXT = enum.auto()


def _flattened_contexts_to_units(
    bundle: Bundle, flattened_contexts: list[PlannedContext]
) -> list[PlannedExecutionUnit]:
    units = []
    current_unit = []

    for planned in flattened_contexts:
        # If we get stdin, start a new execution unit.
        if (
            planned.context.has_main_testcase()
            and cast(MainInput, planned.context.testcases[0].input).stdin
            != EmptyChannel.NONE
        ):
            if current_unit:
                units.append(
                    PlannedExecutionUnit(
                        contexts=current_unit,
                        name=execution_name(bundle.language, len(units)),
                        index=len(units),
                    )
                )
            current_unit = []

        current_unit.append(planned)

        if planned.context.has_exit_testcase():
            units.append(
                PlannedExecutionUnit(
                    contexts=current_unit,
                    name=execution_name(bundle.language, len(units)),
                    index=len(units),
                )
            )
            current_unit = []

    if current_unit:
        units.append(
            PlannedExecutionUnit(
                contexts=current_unit,
                name=execution_name(bundle.language, len(units)),
                index=len(units),
            )
        )

    return units


def plan_test_suite(
    bundle: Bundle, strategy: PlanStrategy
) -> list[PlannedExecutionUnit]:
    """
    Transform a test suite into a list of execution units.

    :param strategy: Which strategy to follow when planning the units.
    :param bundle: The configuration
    :return: A list of planned execution units.
    """

    # First, flatten all contexts into a single list.
    if strategy == PlanStrategy.OPTIMAL:
        flattened_contexts = []
        for t, tab in enumerate(bundle.suite.tabs):
            for c, context in enumerate(tab.contexts):
                flattened_contexts.append(
                    PlannedContext(context=context, tab_index=t, context_index=c)
                )
        flattened_contexts_list = [flattened_contexts]
    elif strategy == PlanStrategy.TAB:
        flattened_contexts_list = []
        for t, tab in enumerate(bundle.suite.tabs):
            flattened_contexts = []
            for c, context in enumerate(tab.contexts):
                flattened_contexts.append(
                    PlannedContext(context=context, tab_index=t, context_index=c)
                )
            flattened_contexts_list.append(flattened_contexts)
    else:
        assert strategy == PlanStrategy.CONTEXT
        flattened_contexts_list = []
        for t, tab in enumerate(bundle.suite.tabs):
            for c, context in enumerate(tab.contexts):
                flattened_contexts = [
                    PlannedContext(context=context, tab_index=t, context_index=c)
                ]
                flattened_contexts_list.append(flattened_contexts)

    nested_units = []
    for flattened_contexts in flattened_contexts_list:
        nested_units.extend(_flattened_contexts_to_units(bundle, flattened_contexts))

    return nested_units
