import itertools
import logging
import shutil
from pathlib import Path

from attrs import define

from tested.configs import Bundle
from tested.dodona import Status
from tested.judge.compilation import process_compile_results, run_compilation
from tested.judge.planning import CompilationResult, ExecutionPlan, PlannedExecutionUnit
from tested.judge.utils import (
    BaseExecutionResult,
    copy_workdir_files,
    filter_files,
    run_command,
)
from tested.languages.conventionalize import selector_name
from tested.languages.preparation import exception_file, value_file
from tested.testsuite import ContentPath
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
    ) -> list[ContextResult]:
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


def execute_file(
    bundle: Bundle,
    executable_name: str,
    working_directory: Path,
    remaining: float | None,
    stdin: str | None = None,
    argument: str | None = None,
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
    _logger.info(f"Starting execution on file {executable_name}")

    command = bundle.language.execution(
        cwd=working_directory,
        file=executable_name,
        arguments=[argument] if argument else [],
    )
    _logger.debug(f"Executing {command} in directory {working_directory}")

    result = run_command(working_directory, remaining, command, stdin)

    assert result is not None
    return result


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
    copy_workdir_files(bundle, execution_dir, True)

    # Copy files from the common directory to the context directory.
    for file in dependencies:
        origin = plan.common_directory / file
        destination = execution_dir / file
        # Ensure we preserve subdirectories.
        destination.parent.mkdir(parents=True, exist_ok=True)
        _logger.debug(f"Copying {origin} to {destination}")
        if origin == destination:
            continue  # Don't copy the file to itself

        # Use hard links instead of copying, due to issues with busy files.
        # See https://github.com/dodona-edu/universal-judge/issues/57
        destination.hardlink_to(origin)

    # Create dynamically generated files if necessary.
    dynamically_generated_file = unit.get_dynamically_generated_files()
    if dynamically_generated_file is not None:
        destination = execution_dir / dynamically_generated_file.path

        if isinstance(dynamically_generated_file.content, ContentPath):
            _logger.debug(
                f"Copying input file %s to %s",
                dynamically_generated_file.content.path,
                destination,
            )
            source_file = (
                bundle.config.resources / dynamically_generated_file.content.path
            )
            shutil.copy2(source_file, destination)
        else:
            _logger.debug(f"Creating dynamically generated file %s", destination)
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_text(dynamically_generated_file.content)

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
) -> ExecutionResult | Status:
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

    executable_or_status = bundle.language.find_main_file(files, main_file_name)
    _logger.debug(f"Searched for main file: {executable_or_status}")
    if isinstance(executable_or_status, Status):
        return executable_or_status

    executable = executable_or_status
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

    return ExecutionResult(
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
