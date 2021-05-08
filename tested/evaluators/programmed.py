"""
Programmed evaluator.
"""
import logging
import traceback
from typing import Tuple, Optional

from . import EvaluationResult, EvaluatorConfig, value
from .utils import cleanup_specific_programmed
from .value import get_values
from ..datatypes import StringTypes
from ..dodona import ExtendedMessage, StatusMessage, Status, Permission
from ..internal_timings import new_stage, end_stage
from ..internationalization import get_i18n_string
from ..judge import evaluate_programmed
from ..judge.utils import BaseExecutionResult
from ..serialisation import StringType, EvalResult, Value
from ..testplan import (TextOutputChannel, FileOutputChannel, ValueOutputChannel,
                        NormalOutputChannel, ExceptionOutputChannel,
                        ProgrammedEvaluator)
from ..utils import Either, get_args

_logger = logging.getLogger(__name__)

DEFAULT_STUDENT = (get_i18n_string("evaluators.programmed.student.default"))


def _maybe_string(value_: str) -> Optional[Value]:
    if value_:
        return StringType(StringTypes.TEXT, value_)
    else:
        return None


def _try_specific(value_: str) -> EvalResult:
    return EvalResult.parse_raw(value_)


def expected_as_value(config: EvaluatorConfig,
                      channel: NormalOutputChannel,
                      actual: str) -> Tuple[Optional[Value], Either[Value]]:
    if isinstance(channel, TextOutputChannel):
        expected = channel.get_data_as_string(config.bundle.config.resources)
        expected_value = _maybe_string(expected)
        actual_value = StringType(StringTypes.TEXT, expected)
        return expected_value, Either(actual_value)

    if isinstance(channel, FileOutputChannel):
        expected = _maybe_string(channel.expected_path)
        actual = StringType(
            type=StringTypes.TEXT,
            data=channel.actual_path
        )
        return expected, Either(actual)

    if isinstance(channel, ValueOutputChannel):
        expected = channel.value
        try:
            actual = value.try_as_value(actual).get()
        except (ValueError, TypeError) as e:
            return expected, Either(e)
        return expected, Either(actual)

    if isinstance(channel, ExceptionOutputChannel):
        raise AssertionError("Programmed evaluation is not support for exceptions.")

    raise AssertionError(f"Unknown channel type for {channel}.")


def evaluate(config: EvaluatorConfig,
             channel: NormalOutputChannel,
             actual: str) -> EvaluationResult:
    """
    Evaluate using a programmed evaluator. This evaluator is unique, in that it is
    also responsible for running the evaluator (all other evaluators don't do that).
    """
    assert isinstance(channel, get_args(NormalOutputChannel))
    assert hasattr(channel, 'evaluator')
    assert isinstance(channel.evaluator, ProgrammedEvaluator)

    _logger.debug(f"Programmed evaluator for output {actual}")

    # Convert the expected item to a Value, which is then passed to the
    # evaluator for evaluation.
    # This is slightly tricky, since the actual value must also be converted
    # to a value, and we are not yet sure what the actual value is exactly
    result = get_values(config.bundle, channel, actual or "")
    # TODO: why is this?
    if isinstance(result, EvaluationResult):
        return result
    else:
        expected, readable_expected, actual, readable_actual = result

    # If there is no actual result, stop early.
    if actual is None:
        return EvaluationResult(
            result=StatusMessage(enum=Status.WRONG),
            readable_expected=readable_expected,
            readable_actual=readable_actual
        )

    _logger.debug(f"Calling programmed evaluation with params:\n"
                  f"expected: {expected}\n"
                  f"actual: {actual}")
    new_stage("evaluate.programmed", True)
    result = evaluate_programmed(
        config.bundle,
        evaluator=channel.evaluator,
        expected=expected,
        actual=actual
    )
    end_stage("evaluate.programmed", True)

    if isinstance(result, BaseExecutionResult):
        if result.timeout:
            return EvaluationResult(
                result=StatusMessage(enum=Status.TIME_LIMIT_EXCEEDED),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=[result.stdout, result.stderr]
            )
        if result.memory:
            return EvaluationResult(
                result=StatusMessage(enum=Status.MEMORY_LIMIT_EXCEEDED),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=[result.stdout, result.stderr]
            )

        if not result.stdout:
            stdout = ExtendedMessage(description=result.stdout, format="text")
            stderr = ExtendedMessage(description=result.stderr, format="text")
            return EvaluationResult(
                result=StatusMessage(enum=Status.INTERNAL_ERROR),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=[stdout, stderr, DEFAULT_STUDENT]
            )
        try:
            evaluation_result = _try_specific(result.stdout)
        except (TypeError, ValueError):
            messages = [
                ExtendedMessage(
                    description=DEFAULT_STUDENT,
                    format="text"
                ),
                ExtendedMessage(
                    description=get_i18n_string("evaluators.programmed.result"),
                    format="text",
                    permission=Permission.STAFF
                ),
                ExtendedMessage(
                    description=traceback.format_exc(),
                    format="code",
                    permission=Permission.STAFF
                ),
                ExtendedMessage(
                    description=get_i18n_string("evaluators.programmed.stdout"),
                    permission=Permission.STAFF
                ),
                ExtendedMessage(
                    description=result.stdout,
                    format="code",
                    permission=Permission.STAFF
                ),
                ExtendedMessage(
                    description=get_i18n_string("evaluators.programmed.stderr"),
                    permission=Permission.STAFF
                ),
                ExtendedMessage(
                    description=result.stderr,
                    format="code",
                    permission=Permission.STAFF
                )
            ]
            return EvaluationResult(
                result=StatusMessage(enum=Status.INTERNAL_ERROR),
                readable_expected=readable_expected,
                readable_actual=readable_actual,
                messages=messages
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
    actual = cleanup_specific_programmed(config, channel, EvalResult(
        result=result_status.enum,
        readable_expected=readable_expected,
        readable_actual=readable_actual,
        messages=evaluation_result.messages
    ))

    return EvaluationResult(
        result=result_status,
        readable_expected=actual.readable_expected,
        readable_actual=actual.readable_actual,
        messages=actual.messages
    )
