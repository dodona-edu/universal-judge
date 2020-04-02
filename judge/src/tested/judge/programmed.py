"""
Programmed evaluation.
"""
import shutil
import time
from pathlib import Path
from typing import Optional

import humps

from tested.configs import Bundle, create_bundle
from tested.judge.core import _logger
from tested.judge.execution import execute_file
from tested.judge.utils import BaseExecutionResult, copy_from_paths_to_path, \
    run_command, find_main_file
from tested.languages.generator import generate_custom_evaluator
from tested.languages.templates import path_to_templates
from tested.serialisation import Value
from tested.testplan import ProgrammedEvaluator
from tested.utils import get_identifier


def evaluate_programmed(bundle: Bundle,
                        evaluator: ProgrammedEvaluator,
                        expected: Optional[Value],
                        actual: Value,
                        timeout: Optional[float]) -> BaseExecutionResult:
    """
    Run the custom evaluation. Concerning structure and execution, the custom
    evaluator is very similar to the execution of the whole evaluation. It a
    mini-evaluation if you will.

    TODO: considering implementing a precompilation mode as well for custom
      evaluators. One difficulty is that there is currently no runtime support
      to decode values, only compile time support.
    """
    start = time.perf_counter()

    # Check if the language supports this.
    if not bundle.language_config.supports_evaluation():
        _logger.error(f"{bundle.config.programming_language} does not support"
                      f" evaluations.")
        return BaseExecutionResult(
            stdout="",
            stderr=f"Evaluatie in {bundle.config.programming_language} wordt niet "
                   f"ondersteund.",
            exit=-1
        )

    # Create a directory for this evaluator. If one exists, delete it first.
    evaluator_dir_name = humps.decamelize(evaluator.path.stem)
    custom_directory_name = f"{get_identifier()}_{evaluator_dir_name}"
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
    remaining = timeout - (time.perf_counter() - start)
    _logger.debug("Compiling custom evaluator with command %s", command)
    result = run_command(custom_path, remaining, command)
    if result and result.stderr:
        raise ValueError("Error while compiling specific test case:" +
                         result.stderr)

    # Execute the custom evaluator.
    evaluator_name = Path(evaluator_name).stem
    executable = find_main_file(files, evaluator_name)
    files.remove(executable)

    remaining = timeout - (time.perf_counter() - start)
    return execute_file(
        bundle=eval_bundle,
        executable_name=executable,
        working_directory=custom_path,
        dependencies=files,
        stdin=None,
        remaining=remaining
    )
