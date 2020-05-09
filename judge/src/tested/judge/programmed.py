"""
Programmed evaluation.
"""
import contextlib
import shutil
import traceback
import types
from dataclasses import dataclass, field
from io import StringIO
from pathlib import Path
from typing import Optional, List, Union, Tuple, ContextManager

import sys

from .core import _logger, ExtendedMessage, Permission
from .execution import execute_file, filter_files
from .utils import (BaseExecutionResult, copy_from_paths_to_path, run_command,
                    find_main_file)
from ..configs import Bundle, create_bundle
from ..dodona import Status
from ..features import Construct
from ..languages.generator import (generate_custom_evaluator, convert_statement,
                                   custom_evaluator_arguments)
from ..languages.templates import path_to_templates
from ..serialisation import Value, EvalResult
from ..testplan import ProgrammedEvaluator
from ..utils import get_identifier


def evaluate_programmed(
        bundle: Bundle,
        evaluator: ProgrammedEvaluator,
        expected: Optional[Value],
        actual: Value) -> Union[BaseExecutionResult, EvalResult]:
    """
    Run the custom evaluation. Concerning structure and execution, the custom
    evaluator is very similar to the execution of the whole evaluation. It a
    mini-evaluation if you will.
    """

    _logger.debug("Doing programmed output")

    # We have special support for Python.
    if evaluator.language == "python" and bundle.config.options.optimized:
        return _evaluate_python(bundle, evaluator, expected, actual)
    else:
        return _evaluate_others(bundle, evaluator, expected, actual)


def _evaluate_others(bundle: Bundle,
                     evaluator: ProgrammedEvaluator,
                     expected: Optional[Value],
                     actual: Value) -> BaseExecutionResult:
    """
    Evaluate in all languages but Python. The re-uses the infrastructure of the
    templates and execution facilities of the language config.
    """
    _logger.debug("Doing evaluation in non-Python mode.")

    # Create a directory for this evaluator. If one exists, delete it first.
    evaluator_dir_name = evaluator.function.file.stem
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
    if Construct.EVALUATION not in eval_bundle.lang_config.supported_constructs():
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
    origin_path = Path(bundle.config.resources, evaluator.function.file)
    _logger.debug("Copying %s to %s", origin_path, custom_path)
    shutil.copy2(origin_path, custom_path)

    # Copy the dependencies to the folder.
    dependencies = eval_bundle.lang_config.initial_dependencies()
    origin = path_to_templates(eval_bundle)
    copy_from_paths_to_path(origin, dependencies, custom_path)
    # Include the actual evaluator in the dependencies.
    dependencies.append(evaluator.function.file.name)

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
    command, files = eval_bundle.lang_config.compilation(dependencies)
    _logger.debug("Compiling custom evaluator with command %s", command)
    result = run_command(custom_path, None, command)
    if result and result.stderr:
        raise ValueError("Error while compiling specific test case:" +
                         result.stderr)

    files = filter_files(files, custom_path)
    # Execute the custom evaluator.
    evaluator_name = Path(evaluator_name).stem
    executable = find_main_file(files, evaluator_name)
    files.remove(executable)

    return execute_file(
        bundle=eval_bundle,
        executable_name=executable,
        working_directory=custom_path,
        stdin=None,
        remaining=None
    )


@contextlib.contextmanager
def _catch_output() -> ContextManager[Tuple[StringIO, StringIO]]:
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    stdout = StringIO()
    stderr = StringIO()
    try:
        sys.stdout = stdout
        sys.stderr = stderr
        yield stdout, stderr
    finally:
        sys.stdout = old_stdout
        sys.stderr = old_stderr


@dataclass
class _EvaluationResult:
    result: bool
    readable_expected: Optional[str] = None
    readable_actual: Optional[str] = None
    messages: List[ExtendedMessage] = field(default_factory=list)


def _evaluate_python(bundle: Bundle,
                     evaluator: ProgrammedEvaluator,
                     expected: Optional[Value],
                     actual: Value) -> EvalResult:
    """
    Run an evaluation in Python. While the templates are still used to generate
    code, they are not executed in a separate process, but inside python itself.
    """
    assert evaluator.language == "python"
    _logger.debug("Doing evaluation in Python mode.")

    # Create a configs bundle for the language of the evaluator.
    eval_bundle = create_bundle(
        bundle.config, bundle.out, bundle.plan, evaluator.language
    )

    # Path to the evaluator.
    origin_path = Path(bundle.config.resources, evaluator.function.file)
    # Read evaluator to file.
    with open(origin_path, "r") as file:
        evaluator_code = file.read()

    # We must provide the globals from the "evaluation_utils" to the code.
    # Begin by defining the module.
    utils = types.ModuleType("evaluation_utils")
    utils.__dict__["EvaluationResult"] = _EvaluationResult
    utils.__dict__["Message"] = ExtendedMessage

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

    with _catch_output() as (stdout_, stderr_):
        exec(
            f"__tested_test__result = {evaluator.function.name}("
            f"{literal_expected}, {literal_actual}, {literal_arguments})",
            global_env
        )

    stdout_ = stdout_.getvalue()
    stderr_ = stderr_.getvalue()

    print(stdout_)
    print(stderr_)

    messages = []
    if stdout_:
        messages.append(ExtendedMessage(
            description="De evaluatiecode produceerde volgende uitvoer op stdout:",
            format="text"
        ))
        messages.append(ExtendedMessage(
            description=stdout_,
            format="code"
        ))
    if stderr_:
        messages.append(ExtendedMessage(
            description="De evaluatiecode produceerde volgende uitvoer op stderr:",
            format="text",
            permission=Permission.STUDENT
        ))
        messages.append(ExtendedMessage(
            description=stderr_,
            format="code",
            permission=Permission.STAFF
        ))

    # noinspection PyTypeChecker
    result_: Optional[_EvaluationResult] = global_env["__tested_test__result"]

    # If the result is None, the evaluator is broken.
    if result_ is None:
        messages.append(ExtendedMessage(
            description="Er ging iets verkeerds tijdens het evalueren van de "
                        "oplossing. Contacteer de lesgever.",
            format="text"
        ))
        messages.append(ExtendedMessage(
            description="De programmeertaalspecifieke evaluatie werkte niet.",
            format="text",
            permission=Permission.STAFF
        ))
        return EvalResult(
            result=Status.INTERNAL_ERROR,
            readable_expected=None,
            readable_actual=None,
            messages=messages
        )

    assert isinstance(result_, _EvaluationResult)

    try:
        return EvalResult(
            result=result_.result,
            readable_expected=result_.readable_expected,
            readable_actual=result_.readable_actual,
            messages=messages + result_.messages
        )
    except (TypeError, ValueError) as e:
        # This happens when the messages are not in the correct format. In normal
        # execution, this is caught when parsing the resulting json, but this does
        # not happen when using Python, so we do it here.
        messages.append(ExtendedMessage(
            description="Er ging iets verkeerds tijdens de evaluatie. Contacteer "
                        "de lesgever.",
            format="text"
        ))
        messages.append(ExtendedMessage(
            description="Het resultaat van de geprogrammeerde evaluatie is "
                        "ongeldig:",
            format="text",
            permission=Permission.STAFF
        ))
        messages.append(ExtendedMessage(
            description=traceback.format_exc(),
            format="code",
            permission=Permission.STAFF
        ))
        return EvalResult(
            result=Status.INTERNAL_ERROR,
            readable_expected=None,
            readable_actual=None,
            messages=messages
        )
