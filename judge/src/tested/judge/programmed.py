"""
Programmed evaluation.
"""
import shutil
import types
from dataclasses import dataclass

import time
from pathlib import Path
from typing import Optional, List, Union

import humps

from tested.configs import Bundle, create_bundle
from tested.judge.core import _logger
from tested.judge.execution import execute_file
from tested.judge.utils import BaseExecutionResult, copy_from_paths_to_path, \
    run_command, find_main_file
from tested.languages.generator import generate_custom_evaluator, \
    convert_statement, custom_evaluator_arguments
from tested.languages.templates import path_to_templates
from tested.serialisation import Value, EvalResult
from tested.testplan import ProgrammedEvaluator
from tested.utils import get_identifier


def evaluate_programmed(
        bundle: Bundle,
        evaluator: ProgrammedEvaluator,
        expected: Optional[Value],
        actual: Value,
        timeout: Optional[float]) -> Union[BaseExecutionResult, EvalResult]:
    """
    Run the custom evaluation. Concerning structure and execution, the custom
    evaluator is very similar to the execution of the whole evaluation. It a
    mini-evaluation if you will.
    """

    # We have special support for Python.
    if evaluator.language == "python" and bundle.config.options.optimized:
        return _evaluate_python(bundle, evaluator, expected, actual, timeout)
    else:
        return _evaluate_others(bundle, evaluator, expected, actual, timeout)


def _evaluate_others(bundle: Bundle,
                     evaluator: ProgrammedEvaluator,
                     expected: Optional[Value],
                     actual: Value,
                     timeout: Optional[float]) -> BaseExecutionResult:
    """
    Evaluate in all languages but Python. The re-uses the infrastructure of the
    templates and execution facilities of the language config.
    """
    _logger.debug("Doing evaluation in non-Python mode.")
    start = time.perf_counter()

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

    # Check if the language supports this.
    if not eval_bundle.language_config.supports_evaluation():
        _logger.error(f"{eval_bundle.config.programming_language} does not support"
                      f" evaluations.")
        return BaseExecutionResult(
            stdout="",
            stderr=f"Evaluatie in {eval_bundle.config.programming_language} wordt "
                   f"niet ondersteund.",
            exit=-1,
            timeout=False,
            memory=False
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
        stdin=None,
        remaining=remaining
    )


def _evaluate_python(bundle: Bundle,
                     evaluator: ProgrammedEvaluator,
                     expected: Optional[Value],
                     actual: Value,
                     timeout: Optional[float]) -> EvalResult:
    """
    Run an evaluation in Python. While the templates are still used to generate
    code, they are not executed in a separate process, but inside python itself.
    """
    assert evaluator.language == "python"
    _logger.debug("Doing evaluation in Python mode.")
    start = time.perf_counter()

    # Create a configs bundle for the language of the evaluator.
    eval_bundle = create_bundle(
        bundle.config, bundle.out, bundle.plan, evaluator.language
    )

    @dataclass
    class EvaluationResult:
        result: bool
        readable_expected: Optional[str] = None
        readable_actual: Optional[str] = None
        messages: Optional[List[str]] = None

    # Path to the evaluator.
    origin_path = Path(bundle.config.resources, evaluator.path)
    # Read evaluator to file.
    with open(origin_path, "r") as file:
        evaluator_code = file.read()

    # We must provide the globals from the "evaluation_utils" to the code.
    # Begin by defining the module.
    utils = types.ModuleType("evaluation_utils")
    utils.__dict__["EvaluationResult"] = EvaluationResult

    # The context in which to execute.
    global_env = {"__tested_test__": utils}
    exec("import sys\n"
         "sys.modules['evaluation_utils'] = __tested_test__", global_env)
    # Make the evaluator available.
    exec(evaluator_code, global_env)

    # Call the evaluator.
    literal_expected = convert_statement(eval_bundle, expected)
    literal_actual = convert_statement(eval_bundle, actual)
    arguments = custom_evaluator_arguments(evaluator)
    literal_arguments = convert_statement(eval_bundle, arguments)

    exec(
        f"__tested_test__result = evaluate(expected={literal_expected}, actual="
        f"{literal_actual}, arguments={literal_arguments})",
        global_env
    )

    # noinspection PyTypeChecker
    result_: EvaluationResult = global_env["__tested_test__result"]

    # If the result is None, the evaluator is broken.
    if result_ is None:
        return EvalResult(
            result=False,
            readable_expected=None,
            readable_actual=None,
            messages=["De evaluator is kapot, contacteer lesgever."]
        )

    assert isinstance(result_, EvaluationResult)
    return EvalResult(
        result=result_.result,
        readable_expected=result_.readable_expected,
        readable_actual=result_.readable_actual,
        messages=result_.messages or []
    )
