import logging
import shutil
import time
from pathlib import Path
from typing import Tuple, List

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

    max_time = float(bundle.config.time_limit) * 0.8
    start = time.perf_counter()

    # Run the linter.
    run_linter(bundle, collector)
    if time.perf_counter() - start > max_time:
        collector.terminate(Status.TIME_LIMIT_EXCEEDED)
        collector.flush(bundle.out)
        exit()

    _logger.info("Start generating code...")
    common_dir, files, selector = _generate_files(bundle, mode)

    # Add the selector to the dependencies.
    if selector:
        files.append(selector)

    if mode == ExecutionMode.PRECOMPILATION:
        assert not bundle.language_config.needs_selector() or selector is not None
        # Compile all code in one go.
        _logger.info("Running precompilation step...")
        result, compilation_files = run_compilation(bundle, common_dir, files)

        messages, status, annotations = process_compile_results(
            bundle.language_config,
            result
        )
        precompilation_result = (messages, status)

        # If we have fallback, discard all results.
        if status != Status.CORRECT and bundle.config.options.allow_fallback:
            mode = ExecutionMode.INDIVIDUAL
            _logger.info("Compilation error, falling back to individual mode")
            # Remove the selector file from the dependencies.
            # Otherwise, it will keep being compiled, which we want to avoid.
            if bundle.language_config.needs_selector():
                files.remove(selector)
        else:
            files = compilation_files
            # Report messages.
            for message in messages:
                collector.add(AppendMessage(message=message))
            for annotation in annotations:
                collector.add(annotation)

            if status != Status.CORRECT:
                collector.add(CloseJudgment(
                    accepted=False,
                    status=StatusMessage(
                        enum=status,
                        human="Ongeldige broncode"
                    )
                ))
                collector.flush(bundle.out)
                _logger.info("Compilation error without fallback")
                return  # Compilation error occurred, useless to continue.
    else:
        precompilation_result = None

    _logger.info("Starting judgement...")

    for tab_index, tab in enumerate(bundle.plan.tabs):
        collector.add(StartTab(title=tab.name), tab_index)
        for context_index, context in enumerate(tab.contexts):
            if time.perf_counter() - start > max_time:
                collector.terminate(Status.TIME_LIMIT_EXCEEDED, tab_index)
                collector.flush(bundle.out)
                exit()
            collector.add(StartContext(description=context.description),
                          context_index)
            execution = ContextExecution(
                context=context,
                context_name=bundle.language_config.context_name(
                    tab_number=tab_index,
                    context_number=context_index
                ),
                mode=mode,
                common_directory=common_dir,
                files=files,
                precompilation_result=precompilation_result,
                collector=collector
            )

            execution_result, m, s, p = execute_context(bundle, execution)

            evaluate_results(
                bundle,
                context=context,
                exec_results=execution_result,
                compiler_results=(m, s),
                context_dir=p,
                collector=collector
            )
            collector.add(CloseContext(), context_index)
        collector.add(CloseTab(), tab_index)
    collector.add(CloseJudgment())
    collector.flush(bundle.out)


def _generate_files(bundle: Bundle,
                    mode: ExecutionMode
                    ) -> Tuple[Path, List[str], Optional[str]]:
    """
    Generate all necessary files, using the templates. This creates a common
    directory, copies all dependencies to that folder and runs the generation.
    """
    dependencies = bundle.language_config.initial_dependencies()
    common_dir = Path(bundle.config.workdir, f"common")
    common_dir.mkdir()

    _logger.debug(f"Generating files in common directory %s", common_dir)

    # Copy dependencies
    dependency_paths = path_to_templates(bundle)
    copy_from_paths_to_path(dependency_paths, dependencies, common_dir)

    submission_name = bundle.language_config.submission_name(bundle.plan)

    # Copy the submission file.
    submission_file = f"{submission_name}" \
                      f".{bundle.language_config.file_extension()}"
    solution_path = common_dir / submission_file
    # noinspection PyTypeChecker
    shutil.copy2(bundle.config.source, solution_path)
    dependencies.append(submission_file)

    # Allow modifications of the submission file.
    bundle.language_config.solution_callback(solution_path, bundle)

    # The names of the contexts in the testplan.
    context_names = []
    # Generate the files for each context.
    for tab_i, tab in enumerate(bundle.plan.tabs):
        for context_i, context in enumerate(tab.contexts):
            context_name = bundle.language_config.context_name(tab_i, context_i)
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
            and bundle.language_config.needs_selector():
        _logger.debug("Generating selector for PRECOMPILATION mode.")
        generated = generate_selector(bundle, common_dir, context_names)
    else:
        generated = None
    return common_dir, dependencies, generated
