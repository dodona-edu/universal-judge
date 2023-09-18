import logging
import shutil
import time
from pathlib import Path

from tested.configs import Bundle
from tested.dodona import (
    CloseContext,
    CloseJudgement,
    CloseTab,
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
    ExecutionResult,
    PlanStrategy,
    compile_unit,
    execute_unit,
    filter_files,
    plan_test_suite,
    set_up_unit,
)
from tested.judge.linter import run_linter
from tested.judge.planning import (
    CompilationResult,
    ExecutionPlan,
    PlannedContext,
    PlannedExecutionUnit,
)
from tested.judge.utils import copy_from_paths_to_path
from tested.languages.conventionalize import EXECUTION_PREFIX, submission_file
from tested.languages.generation import generate_execution, generate_selector

_logger = logging.getLogger(__name__)


def _is_fatal_compilation_error(compilation_results: CompilationResult) -> bool:
    return compilation_results.status in (
        Status.TIME_LIMIT_EXCEEDED,
        Status.MEMORY_LIMIT_EXCEEDED,
    )


def _handle_time_or_memory_compilation(
    bundle: Bundle, collector: OutputManager, results: CompilationResult
):
    assert _is_fatal_compilation_error(results)
    collector.add_messages(results.messages)
    collector.add_all(results.annotations)
    terminate(
        bundle,
        collector,
        StatusMessage(
            enum=results.status,
            human=get_i18n_string("judge.core.invalid.source-code"),
        ),
    )


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
    if not is_supported(bundle.language):
        report_update(bundle.out, StartJudgement())
        report_update(
            bundle.out,
            CloseJudgement(
                status=StatusMessage(
                    enum=Status.INTERNAL_ERROR,
                    human=get_i18n_string("judge.core.unsupported.language"),
                ),
            ),
        )
        _logger.info("Required features not supported.")
        return  # Not all required features are supported.

    # Do the set-up for the judgement.
    collector = OutputManager(bundle.out)
    collector.add(StartJudgement())
    max_time = float(bundle.config.time_limit) * 0.9
    start = time.perf_counter()

    # Run the linter.
    # TODO: do this in parallel
    run_linter(bundle, collector, max_time)
    if time.perf_counter() - start > max_time:
        terminate(bundle, collector, Status.TIME_LIMIT_EXCEEDED)
        return

    planned_units = plan_test_suite(bundle, strategy=PlanStrategy.OPTIMAL)

    # Attempt to precompile everything.
    common_dir, dependencies, selector = _generate_files(bundle, planned_units)

    # Create an execution plan.
    plan = ExecutionPlan(
        units=planned_units,
        common_directory=common_dir,
        files=dependencies,
        selector=selector,
        max_time=max_time,
        start_time=start,
    )

    _logger.debug("Attempting precompilation")
    compilation_results = precompile(bundle, plan)

    # If something went horribly wrong, and the compilation itself caused a timeout or memory issue, bail now.
    if _is_fatal_compilation_error(compilation_results):
        _handle_time_or_memory_compilation(bundle, collector, compilation_results)
        return

    # If the compilation failed, but we can fall back, do that.
    if (
        compilation_results.status != Status.CORRECT
        and bundle.config.options.allow_fallback
    ):
        _logger.warning("Precompilation failed. Falling back to unit compilation.")
        planned_units = plan_test_suite(bundle, strategy=PlanStrategy.TAB)
        plan.units = planned_units
        compilation_results = None

    _judge_planned_units(bundle, collector, plan, compilation_results)


def _judge_planned_units(
    bundle: Bundle,
    collector: OutputManager,
    plan: ExecutionPlan,
    compilation_results: CompilationResult | None,
):
    _logger.info("Starting execution.")
    currently_open_tab = -1
    # Create a list of runs we want to execute.
    for i, planned_unit in enumerate(plan.units):
        # Prepare the unit.
        execution_dir, dependencies = set_up_unit(bundle, plan, i)

        # If compilation is necessary, do it.
        if compilation_results is None:
            local_compilation_results, dependencies = compile_unit(
                bundle, plan, i, execution_dir, dependencies
            )
            if _is_fatal_compilation_error(local_compilation_results):
                _handle_time_or_memory_compilation(
                    bundle, collector, local_compilation_results
                )
                return
        else:
            local_compilation_results = compilation_results

        # Execute the unit.
        if local_compilation_results.status == Status.CORRECT:
            remaining_time = plan.remaining_time()
            execution_result, status = execute_unit(
                bundle, planned_unit, execution_dir, dependencies, remaining_time
            )
        else:
            execution_result = None

        result_status, currently_open_tab = _process_results(
            bundle=bundle,
            unit=planned_unit,
            execution_result=execution_result,
            execution_dir=execution_dir,
            compilation_results=local_compilation_results,
            collector=collector,
            currently_open_tab=currently_open_tab,
        )

        if result_status in (
            Status.TIME_LIMIT_EXCEEDED,
            Status.MEMORY_LIMIT_EXCEEDED,
            Status.OUTPUT_LIMIT_EXCEEDED,
        ):
            terminate(bundle, collector, result_status)
            return

    # Close the last tab.
    collector.add(CloseTab(), currently_open_tab)
    collector.add(CloseJudgement())


def precompile(bundle: Bundle, plan: ExecutionPlan) -> CompilationResult:
    """
    Attempt to precompile the execution plan.

    :param bundle: The options.
    :param plan: The execution plan.
    :return: The results of the precompilation step.
    """
    _logger.info("Starting precompilation phase")
    assert not bundle.language.needs_selector() or plan.selector is not None
    plan_files = filter_files(plan.files, plan.common_directory)
    files = _copy_workdir_source_files(bundle, plan.common_directory) + [
        str(x) for x in plan_files
    ]
    remaining_time = plan.remaining_time()

    # Do the actual compiling.
    result, compilation_files = run_compilation(
        bundle, plan.common_directory, files, remaining_time
    )

    # Update the files if the compilation succeeded.
    processed_results = process_compile_results(bundle.language, result)
    if processed_results.status == Status.CORRECT:
        plan.files = compilation_files

    return processed_results


def _generate_files(
    bundle: Bundle, execution_plan: list[PlannedExecutionUnit]
) -> tuple[Path, list[str], str | None]:
    """
    Generate all necessary files, using the templates. This creates a common
    directory, copies all dependencies to that folder and runs the generation.
    """
    dependencies = bundle.language.initial_dependencies()
    common_dir = Path(bundle.config.workdir, f"common")
    common_dir.mkdir()

    _logger.debug(f"Generating files in common directory {common_dir}")

    # Copy dependencies
    dependency_paths = bundle.language.path_to_dependencies()
    copy_from_paths_to_path(dependency_paths, dependencies, common_dir)

    # Copy the submission file.
    submission = submission_file(bundle.language)
    solution_path = common_dir / submission
    # noinspection PyTypeChecker
    shutil.copy2(bundle.config.source, solution_path)
    dependencies.append(submission)

    # Allow modifications of the submission file.
    bundle.language.modify_solution(solution_path)

    # The names of the executions for the test suite.
    execution_names = []
    # Generate the files for each execution.
    for execution_unit in execution_plan:
        _logger.debug(f"Generating file for execution {execution_unit.name}")
        generated, evaluators = generate_execution(
            bundle=bundle, destination=common_dir, execution_unit=execution_unit
        )

        # Copy functions to the directory.
        for evaluator in evaluators:
            source = Path(bundle.config.resources) / evaluator
            _logger.debug(f"Copying oracle from {source} to {common_dir}")
            shutil.copy2(source, common_dir)

        dependencies.extend(evaluators)
        dependencies.append(generated)
        execution_names.append(execution_unit.name)

    if bundle.language.needs_selector():
        _logger.debug("Generating selector.")
        generated = generate_selector(bundle, common_dir, execution_names)
        dependencies.append(generated)
    else:
        generated = None
    return common_dir, dependencies, generated


def _process_results(
    bundle: Bundle,
    collector: OutputManager,
    unit: PlannedExecutionUnit,
    compilation_results: CompilationResult,
    execution_result: ExecutionResult | None,
    execution_dir: Path,
    currently_open_tab: int,
) -> tuple[Status | None, int]:
    if execution_result:
        context_results = execution_result.to_context_results()
    else:
        context_results = [None] * len(unit.contexts)

    for planned, context_result in zip(unit.contexts, context_results):
        planned: PlannedContext
        if currently_open_tab < planned.tab_index:
            # Close the previous tab if necessary.
            if collector.open_stack[-1] == "tab":
                collector.add(CloseTab(), currently_open_tab)
            currently_open_tab = currently_open_tab + 1
            tab = bundle.suite.tabs[currently_open_tab]
            collector.add(StartTab(title=tab.name, hidden=tab.hidden))

        # Handle the contexts.
        collector.add(StartContext(description=planned.context.description))

        continue_ = evaluate_context_results(
            bundle,
            context=planned.context,
            exec_results=context_result,
            context_dir=execution_dir,
            collector=collector,
            compilation_results=compilation_results,
        )

        collector.add(CloseContext(), planned.context_index)
        if continue_ in (Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED):
            return continue_, currently_open_tab

    return None, currently_open_tab


def _copy_workdir_source_files(bundle: Bundle, common_dir: Path) -> list[str]:
    """
    Copy additional source files from the workdir to the common dir

    :param bundle: Bundle information of the test suite
    :param common_dir: The directory of the other files
    """
    source_files = []

    def recursive_copy(src: Path, dst: Path):
        for origin in src.iterdir():
            file = origin.name.lower()
            if origin.is_file() and bundle.language.is_source_file(origin):
                source_files.append(str(dst / origin.name))
                _logger.debug(f"Copying {origin} to {dst}")
                shutil.copy2(origin, dst)
            elif (
                origin.is_dir()
                and not file.startswith(EXECUTION_PREFIX)
                and file != "common"
            ):
                _logger.debug(f"Iterate subdir {dst / file}")
                shutil.copytree(origin, dst / file)

    recursive_copy(bundle.config.workdir, common_dir)

    return source_files
