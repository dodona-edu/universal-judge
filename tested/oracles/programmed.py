import logging
import traceback
from typing import List

from tested.dodona import ExtendedMessage, Message, Permission, Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.judge.programmed import evaluate_programmed
from tested.judge.utils import BaseExecutionResult
from tested.oracles.common import (
    OracleConfig,
    OracleResult,
    cleanup_specific_programmed,
)
from tested.oracles.value import get_values
from tested.parsing import get_converter
from tested.serialisation import BooleanEvalResult, EvalResult, StringType, Value
from tested.testsuite import CustomCheckOracle, OracleOutputChannel, OutputChannel

_logger = logging.getLogger(__name__)

DEFAULT_STUDENT = get_i18n_string("evaluators.programmed.student.default")


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
    # TODO: why is this?
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
    result = evaluate_programmed(
        config.bundle, evaluator=channel.oracle, expected=expected, actual=actual
    )

    if isinstance(result, BaseExecutionResult):
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
            evaluation_result = (
                get_converter().loads(result.stdout, BooleanEvalResult).as_eval_result()
            )
        except Exception as e:
            _logger.exception(e)
            messages: List[Message] = [
                ExtendedMessage(description=DEFAULT_STUDENT, format="text"),
                ExtendedMessage(
                    description=get_i18n_string("evaluators.programmed.result"),
                    format="text",
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=traceback.format_exc(),
                    format="code",
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=get_i18n_string("evaluators.programmed.stdout"),
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=result.stdout,
                    format="code",
                    permission=Permission.STAFF,
                ),
                ExtendedMessage(
                    description=get_i18n_string("evaluators.programmed.stderr"),
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
        assert isinstance(result, EvalResult)
        evaluation_result = result

    if evaluation_result.readable_expected:
        readable_expected = evaluation_result.readable_expected
    if evaluation_result.readable_actual:
        readable_actual = evaluation_result.readable_actual

    if isinstance(evaluation_result.result, Status):
        result_status = StatusMessage(enum=evaluation_result.result)
    else:
        assert isinstance(evaluation_result.result, bool)
        result_status = StatusMessage(
            enum=Status.CORRECT if evaluation_result.result else Status.WRONG
        )
    cleaned = cleanup_specific_programmed(
        config,
        channel,
        EvalResult(
            result=result_status.enum,
            readable_expected=readable_expected,  # type: ignore
            readable_actual=readable_actual,  # type: ignore
            messages=evaluation_result.messages,
        ),
    )

    return OracleResult(
        result=result_status,
        readable_expected=cleaned.readable_expected or "",
        readable_actual=cleaned.readable_actual or "",
        messages=cleaned.messages,
    )