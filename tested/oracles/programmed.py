import contextlib
import logging
import sys
import traceback
from io import StringIO
from pathlib import Path
from types import ModuleType
from typing import Any, Generator, cast

from attrs import define

from tested.configs import Bundle, create_bundle
from tested.dodona import ExtendedMessage, Message, Permission, Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.judge.utils import BaseExecutionResult
from tested.languages.generation import generate_statement
from tested.oracles.common import (
    BooleanEvalResult,
    OracleConfig,
    OracleContext,
    OracleResult,
)
from tested.oracles.value import get_values
from tested.parsing import get_converter
from tested.serialisation import FunctionCall, FunctionType, Identifier
from tested.testsuite import CustomCheckOracle, OracleOutputChannel, OutputChannel

_logger = logging.getLogger(__name__)

DEFAULT_STUDENT = get_i18n_string("oracles.programmed.student.default")


@define
class ConvertedOracleContext:
    expected: Any
    actual: Any
    execution_directory: str
    evaluation_directory: str
    programming_language: str
    natural_language: str

    @staticmethod
    def from_context(
        bundle: Bundle, context: OracleContext
    ) -> "ConvertedOracleContext":
        return ConvertedOracleContext(
            expected=eval(generate_statement(bundle, context.expected)),
            actual=eval(generate_statement(bundle, context.actual)),
            execution_directory=context.execution_directory,
            evaluation_directory=context.evaluation_directory,
            programming_language=context.programming_language,
            natural_language=context.natural_language,
        )


@contextlib.contextmanager
def _catch_output() -> Generator[tuple[StringIO, StringIO], None, None]:
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


def _evaluate_programmed(
    bundle: Bundle,
    oracle: CustomCheckOracle,
    context: OracleContext,
) -> BaseExecutionResult | BooleanEvalResult:
    """
    Run the custom evaluation. Concerning structure and execution, the custom
    oracle is very similar to the execution of the whole evaluation. It a
    mini-evaluation if you will.
    """
    _logger.debug("Doing evaluation in Python mode.")

    # Create a configs bundle for the language of the oracle.
    eval_bundle = create_bundle(bundle.config, bundle.out, bundle.suite, "python")

    # Path to the oracle.
    origin_path = Path(bundle.config.resources, oracle.function.file)
    # Read oracle to file.
    with open(origin_path, "r") as file:
        evaluator_code = file.read()

    # We must provide the globals from the "evaluation_utils" to the code.
    # Begin by defining the module.
    utils = ModuleType("evaluation_utils")
    utils.__dict__["EvaluationResult"] = BooleanEvalResult
    utils.__dict__["Message"] = ExtendedMessage
    utils.__dict__["ConvertedOracleContext"] = ConvertedOracleContext

    # The context in which to execute.
    global_env = {
        "__tested_test__": utils,
        "__tested_context__": ConvertedOracleContext.from_context(eval_bundle, context),
    }
    exec("import sys\n" "sys.modules['evaluation_utils'] = __tested_test__", global_env)
    # Make the oracle available.
    exec(evaluator_code, global_env)

    # Since we pass a class value, we don't want to
    check_function_call = FunctionCall(
        type=FunctionType.FUNCTION,
        name=oracle.function.name,
        arguments=[Identifier("__tested_context__"), *oracle.arguments],
    )
    literal_function_call = generate_statement(eval_bundle, check_function_call)

    messages = []
    # noinspection PyBroadException
    try:
        with _catch_output() as (stdout_, stderr_):
            exec(f"__tested_test__result = {literal_function_call}", global_env)
        result_ = cast(BooleanEvalResult | None, global_env["__tested_test__result"])
    except Exception as e:
        _logger.exception(e)
        result_ = None
        messages.append(
            ExtendedMessage(
                description="The custom check oracle failed with the following exception:",
                format="text",
                permission=Permission.STAFF,
            )
        )
        tb = traceback.format_exc()
        messages.append(
            ExtendedMessage(description=tb, format="code", permission=Permission.STAFF)
        )

    stdout_ = stdout_.getvalue()
    stderr_ = stderr_.getvalue()

    if stdout_:
        messages.append(
            ExtendedMessage(
                description=get_i18n_string("judge.programmed.produced.stdout"),
                format="text",
            )
        )
        messages.append(ExtendedMessage(description=stdout_, format="code"))
    if stderr_:
        messages.append(
            ExtendedMessage(
                description=get_i18n_string("judge.programmed.produced.stderr"),
                format="text",
                permission=Permission.STUDENT,
            )
        )
        messages.append(
            ExtendedMessage(
                description=stderr_, format="code", permission=Permission.STAFF
            )
        )

    # If the result is None, the oracle is broken.
    if result_ is None:
        messages.append(
            ExtendedMessage(
                description=get_i18n_string("judge.programmed.student"), format="text"
            )
        )
        messages.append(
            ExtendedMessage(
                description=get_i18n_string("judge.programmed.failed"),
                format="text",
                permission=Permission.STAFF,
            )
        )
        return BooleanEvalResult(
            result=Status.INTERNAL_ERROR,
            readable_expected=None,
            readable_actual=None,
            messages=messages,
        )

    result_.messages.extend(messages)
    return result_


def evaluate(
    config: OracleConfig, channel: OutputChannel, actual_str: str
) -> OracleResult:
    """
    Evaluate using a programmed oracle. This oracle is unique, in that it is
    also responsible for running the oracle (all other functions don't do that).
    """
    assert isinstance(channel, OracleOutputChannel)
    assert isinstance(channel.oracle, CustomCheckOracle)

    _logger.debug(f"Programmed oracle for output {actual_str}")

    # Convert the expected item to a Value, which is then passed to the
    # oracle for evaluation.
    # This is slightly tricky, since the actual value must also be converted
    # to a value, and we are not yet sure what the actual value is exactly
    result = get_values(config.bundle, channel, actual_str or "")
    # If an error occurred, we get a result directly.
    if isinstance(result, OracleResult):
        return result
    else:
        expected, readable_expected, actual, readable_actual = result

    # If there is no actual result, stop early.
    if actual is None:
        return OracleResult(
            result=StatusMessage(enum=Status.WRONG),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
        )

    _logger.debug(
        f"Calling programmed evaluation with params:\n"
        f"expected: {expected}\n"
        f"actual: {actual}"
    )
    context = OracleContext(
        expected=expected,
        actual=actual,
        execution_directory=str(config.context_dir.absolute()),
        evaluation_directory=str(config.bundle.config.resources.absolute()),
        programming_language=str(config.bundle.config.programming_language),
        natural_language=config.bundle.config.natural_language,
    )
    result = _evaluate_programmed(config.bundle, channel.oracle, context)

    if isinstance(result, BaseExecutionResult):
        _logger.error(result.stderr)
        if result.timeout:
            return OracleResult(
                result=StatusMessage(enum=Status.TIME_LIMIT_EXCEEDED),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=[result.stdout, result.stderr],
            )
        if result.memory:
            return OracleResult(
                result=StatusMessage(enum=Status.MEMORY_LIMIT_EXCEEDED),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=[result.stdout, result.stderr],
            )

        if not result.stdout:
            stdout = ExtendedMessage(description=result.stdout, format="text")
            stderr = ExtendedMessage(description=result.stderr, format="text")
            return OracleResult(
                result=StatusMessage(enum=Status.INTERNAL_ERROR),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=[stdout, stderr, DEFAULT_STUDENT],
            )
        try:
            evaluation_result = get_converter().loads(result.stdout, BooleanEvalResult)
        except Exception as e:
            _logger.exception(e)
            messages: list[Message] = [
                ExtendedMessage(description=DEFAULT_STUDENT, format="text"),
                ExtendedMessage(
                    description=get_i18n_string("oracles.programmed.result"),
                    format="text",
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=traceback.format_exc(),
                    format="code",
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=get_i18n_string("oracles.programmed.stdout"),
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=result.stdout,
                    format="code",
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=get_i18n_string("oracles.programmed.stderr"),
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=result.stderr,
                    format="code",
                    permission=Permission.STAFF,
                ),
            ]
            return OracleResult(
                result=StatusMessage(enum=Status.INTERNAL_ERROR),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=messages,
            )
    else:
        assert isinstance(result, BooleanEvalResult)
        evaluation_result = result

    return evaluation_result.to_oracle_result(
        config.bundle, channel, readable_actual, readable_expected
    )
