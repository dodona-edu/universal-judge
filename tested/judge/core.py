import logging
import shutil
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from tested.configs import Bundle
from tested.dodona import (
    AppendMessage,
    CloseContext,
    CloseJudgement,
    CloseTab,
    Metadata,
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
from tested.judge.compilation import precompile
from tested.judge.evaluation import evaluate_context_results, terminate
from tested.judge.execution import (
    ExecutionResult,
    compile_unit,
    execute_unit,
    set_up_unit,
)
from tested.judge.linter import run_linter
from tested.judge.planning import (
    CompilationResult,
    ExecutionPlan,
    PlannedContext,
    PlannedExecutionUnit,
    PlanStrategy,
    plan_test_suite,
)
from tested.judge.utils import copy_from_paths_to_path
from tested.languages.conventionalize import submission_file
from tested.languages.generation import (
    generate_execution,
    generate_selector,
    generate_statement,
)
from tested.serialisation import Statement
from tested.testsuite import LanguageLiterals, MainInput, TextData

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

    The current strategy for executing is as follows:

    1. Convert all contexts into as little units as possible.
    2. Attempt to precompile everything.
       a. If this fails, go to 3.
       b. If this succeeds, go to 4.
    3. Convert contexts into "tab-level" units.
    4. For each execution unit:
       a. Compile if necessary (only if 2 failed)
       b. Execute the unit.
       c. Process the results.

    :param bundle: The configuration bundle.
    """
    # Begin by checking if the given test suite is executable in this language.
    _logger.info("Checking supported features...")
    set_locale(bundle.config.natural_language)
    if messages := is_supported(bundle.language):
        report_update(bundle.out, StartJudgement())
        for message in messages:
            report_update(bundle.out, AppendMessage(message=message))
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
    if bundle.messages:
        collector.add_messages(bundle.messages)

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

    _logger.info("Starting execution")

    def _process_one_unit(
        index: int,
    ) -> tuple[CompilationResult, ExecutionResult | None, Path]:
        return _execute_one_unit(bundle, plan, compilation_results, index)

    if bundle.config.options.parallel:
        max_workers = None
    else:
        max_workers = 1

    _logger.debug(f"Executing with {max_workers} workers")

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        remaining_time = plan.remaining_time()
        results = executor.map(
            _process_one_unit, range(len(plan.units)), timeout=remaining_time
        )
        try:
            currently_open_tab = -1
            for i, (
                local_compilation_results,
                execution_result,
                execution_dir,
            ) in enumerate(results):
                planned_unit = plan.units[i]
                _logger.debug(f"Processing results for execution unit {i}")
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
                    del results
                    terminate(bundle, collector, result_status)
                    return
        except TimeoutError:
            terminate(bundle, collector, Status.TIME_LIMIT_EXCEEDED)
            return

    # Close the last tab.
    terminate(bundle, collector, Status.CORRECT)


def _execute_one_unit(
    bundle: Bundle,
    plan: ExecutionPlan,
    compilation_results: CompilationResult | None,
    index: int,
) -> tuple[CompilationResult, ExecutionResult | None, Path]:
    planned_unit = plan.units[index]
    # Prepare the unit.
    execution_dir, dependencies = set_up_unit(bundle, plan, index)

    # If compilation is necessary, do it.
    if compilation_results is None:
        local_compilation_results, dependencies = compile_unit(
            bundle, plan, index, execution_dir, dependencies
        )
    else:
        local_compilation_results = compilation_results

    # Execute the unit.
    if local_compilation_results.status == Status.CORRECT:
        remaining_time = plan.remaining_time()
        execution_result_or_status = execute_unit(
            bundle, planned_unit, execution_dir, dependencies, remaining_time
        )
        if isinstance(execution_result_or_status, Status):
            local_compilation_results.status = execution_result_or_status
            execution_result = None
        else:
            execution_result = execution_result_or_status
    else:
        execution_result = None

    return local_compilation_results, execution_result, execution_dir


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

        continue_, seen_files = evaluate_context_results(
            bundle,
            context=planned.context,
            exec_results=context_result,
            context_dir=execution_dir,
            collector=collector,
            compilation_results=compilation_results,
        )

        if bundle.language.supports_debug_information():
            # TODO: this is currently very Python-specific
            # See if we need a callback to the language modules in the future.
            # TODO: we could probably re-use the "readable_input" function here,
            #       since it only differs a bit.
            meta_statements = []
            input_files = []
            meta_stdin = None
            for file in seen_files:
                file_data = {"path": file.path}
                if file.content != "":
                    file_data["content"] = file.content

                input_files.append(file_data)

            for case in planned.context.testcases:
                if case.is_main_testcase():
                    assert isinstance(case.input, MainInput)
                    if isinstance(case.input.stdin, TextData):
                        meta_stdin = case.input.stdin.get_data_as_string(
                            bundle.config.resources
                        )
                elif isinstance(case.input, Statement):
                    stmt = generate_statement(bundle, case.input)
                    meta_statements.append(stmt)
                elif isinstance(case.input, LanguageLiterals):
                    stmt = case.input.get_for(bundle.config.programming_language)
                    meta_statements.append(stmt)
                elif not case.is_main_testcase():
                    raise AssertionError(f"Found unknown case input type: {case.input}")

            if meta_statements:
                meta_statements = "\n".join(meta_statements)
            else:
                # Don't add empty statements
                meta_statements = None

            if not input_files:
                input_files = None

            collector.add(
                CloseContext(
                    data=Metadata(
                        statements=meta_statements, files=input_files, stdin=meta_stdin
                    )
                ),
                planned.context_index,
            )
        else:
            collector.add(CloseContext(), planned.context_index)
        if continue_ in (Status.TIME_LIMIT_EXCEEDED, Status.MEMORY_LIMIT_EXCEEDED):
            return continue_, currently_open_tab

    return None, currently_open_tab
