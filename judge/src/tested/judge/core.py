"""
The main judge package. Responsible for turning a configuration bundle (containing
the configs, testplan and solution) into output for Dodona.

The main module has two functions, which are the main interfaces to interact with
the judge. All other modules might be useful, but are more for the internal code
organisation.
"""
import logging
import shutil
from pathlib import Path
from typing import Tuple, List

import humps

from .compilation import run_compilation, process_compile_results
from .evaluation import evaluate_results, execute_context
from .execution import ContextExecution, execute_file
from .linter import run_linter
from .utils import BaseExecutionResult, run_command, find_main_file, \
    copy_from_paths_to_path
from ..configs import Bundle, create_bundle
from ..dodona import *
from ..features import is_supported
from ..languages.generator import generate_context, generate_selector, \
    generate_custom_evaluator
from ..languages.templates import path_to_templates
from ..serialisation import Value
from ..testplan import ExecutionMode, ProgrammedEvaluator
from ..utils import get_identifier

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

    mode = bundle.plan.configuration.mode
    report_update(bundle.out, StartJudgment())

    # Run the linter.
    run_linter(bundle)

    _logger.info("Start generating code...")
    common_dir, files, selector = generate_files(bundle, mode)

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
        if status != Status.CORRECT and bundle.plan.configuration.allow_fallback:
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
                report_update(bundle.out, AppendMessage(message=message))
            for annotation in annotations:
                report_update(bundle.out, annotation)

            if status != Status.CORRECT:
                report_update(bundle.out, CloseJudgment(
                    accepted=False,
                    status=StatusMessage(
                        enum=status,
                        human="Ongeldige broncode"
                    )
                ))
                _logger.info("Compilation error without fallback")
                return  # Compilation error occurred, useless to continue.
    else:
        precompilation_result = None

    _logger.info("Starting judgement...")
    # pool = Pool(4 if plan.configuration.parallel else 1)

    # with utils.protected_directory(common_dir) as common_dir:

    for tab_index, tab in enumerate(bundle.plan.tabs):
        report_update(bundle.out, StartTab(title=tab.name))
        # Create a list of arguments to execute_module (in threads)
        executions = []
        for context_index, context in enumerate(tab.contexts):
            executions.append(ContextExecution(
                context=context,
                context_name=bundle.language_config.context_name(
                    tab_number=tab_index,
                    context_number=context_index
                ),
                mode=mode,
                common_directory=common_dir,
                files=files,
                precompilation_result=precompilation_result
            ))

        results = []
        for execution in executions:
            results.append(execute_context(bundle, execution))
        # Do the executions in parallel
        # results = pool.map(self.execute_context, executions)

        # Handle the results
        for context_index, context in enumerate(tab.contexts):
            report_update(bundle.out, StartContext(
                description=context.description
            ))
            execution_result, m, s, p = results[context_index]
            evaluate_results(
                bundle,
                context=context,
                exec_results=execution_result,
                compiler_results=(m, s),
                context_dir=p
            )
            report_update(bundle.out, CloseContext())
        report_update(bundle.out, CloseTab())
    report_update(bundle.out, CloseJudgment())


def generate_files(bundle: Bundle,
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
    shutil.copy2(bundle.config.source, solution_path)
    dependencies.append(submission_file)

    # Allow modifications of the submission file.
    bundle.language_config.solution_callback(solution_path, bundle.plan)

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


def evaluate_programmed(bundle: Bundle,
                        evaluator: ProgrammedEvaluator,
                        expected: Optional[Value],
                        actual: Value) -> BaseExecutionResult:
    """
    Run the custom evaluation. Concerning structure and execution, the custom
    evaluator is very similar to the execution of the whole evaluation. It a
    mini-evaluation if you will.

    TODO: considering implementing a precompilation mode as well for custom
      evaluators. One difficulty is that there is currently no runtime support
      to decode values, only compile time support.
    """

    # Create a directory for this evaluator. If one exists, delete it first.
    evaluator_dir_name = humps.decamelize(evaluator.path.stem)
    custom_directory_name = f"{evaluator_dir_name}_{get_identifier()}"
    custom_path = Path(bundle.config.workdir, "evaluators", custom_directory_name)
    if custom_path.exists():
        _logger.debug("Removing existing directory for custom evaluator.")
        shutil.rmtree(custom_path, ignore_errors=True)
    custom_path.mkdir(parents=True)

    _logger.info("Will do custom evaluation in %s", custom_path)

    # Create a configs bundle for the language of the evaluator.
    eval_bundle = create_bundle(
        bundle.config, bundle.out, bundle.plan, evaluator.language
    )

    # Copy the evaluator
    origin_path = Path(bundle.config.resources, evaluator.path)
    _logger.debug("Copying %s to %s", origin_path, custom_path)
    shutil.copy2(origin_path, custom_path)

    # Copy the dependencies to the folder.
    dependencies = eval_bundle.language_config.initial_dependencies()
    dependencies.extend(eval_bundle.language_config.evaluator_dependencies())
    origin = path_to_templates(eval_bundle)
    copy_from_paths_to_path(origin, dependencies, custom_path)
    # Include the actual evaluator in the dependencies.
    dependencies.append(evaluator.path.name)

    # Generate the evaluator.
    _logger.debug("Generating custom evaluator.")
    evaluator_name = generate_custom_evaluator(
        eval_bundle,
        destination=custom_path,
        evaluator=evaluator,
        expected_value=expected,
        actual_value=actual
    )
    dependencies.append(evaluator_name)
    _logger.debug("Generated evaluator executor %s", evaluator_name)

    # Do compilation for those configs that require it.
    command, files = eval_bundle.language_config.evaluator_generation_callback(
        dependencies
    )
    _logger.debug("Compiling custom evaluator with command %s", command)
    result = run_command(custom_path, command)
    if result and result.stderr:
        raise ValueError("Error while compiling specific test case:" +
                         result.stderr)

    # Execute the custom evaluator.
    evaluator_name = Path(evaluator_name).stem
    executable = find_main_file(files, evaluator_name)
    files.remove(executable)

    return execute_file(
        bundle=eval_bundle,
        executable_name=executable,
        working_directory=custom_path,
        dependencies=files,
        stdin=None
    )
