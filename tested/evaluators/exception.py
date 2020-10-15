"""
Exception evaluator.
"""
import logging
from typing import Optional

from . import EvaluationResult, EvaluatorConfig
from ..dodona import StatusMessage, Status, ExtendedMessage, Permission
from ..serialisation import ExceptionValue
from ..testplan import ExceptionOutputChannel
from ..utils import Either

logger = logging.getLogger(__name__)


def try_as_exception(config: EvaluatorConfig,
                     value: str) -> Either[ExceptionValue]:
    try:
        actual = ExceptionValue.parse_raw(value)
        actual = config.bundle.lang_config.exception_output(config.bundle, actual)
        return Either(actual)
    except (TypeError, ValueError) as e:
        return Either(e)


def try_as_readable_exception(config: EvaluatorConfig,
                              value: str) -> Optional[str]:
    try:
        actual = ExceptionValue.parse_raw(value)
        actual = config.bundle.lang_config.exception_output(config.bundle, actual)
    except (TypeError, ValueError):
        return None
    else:
        return actual.readable()


def evaluate(config: EvaluatorConfig,
             channel: ExceptionOutputChannel,
             actual: str) -> EvaluationResult:
    """
    Evaluate an exception.

    :param config: Not used.
    :param channel: The channel from the testplan.
    :param actual: The raw actual value from the execution.

    :return: An evaluation result.
    """
    assert isinstance(channel, ExceptionOutputChannel)
    assert channel.exception is not None

    expected = channel.exception

    try:
        actual = try_as_exception(config, actual).get()
    except (TypeError, ValueError) as e:
        staff_message = ExtendedMessage(
            description=f"Expected value exception, but received {actual}, which "
                        f"caused an exception while parsing: {e}",
            format="text",
            permission=Permission.STAFF
        )
        student_message = ("Dodona verstond de exception niet. Meld deze fout aan "
                           "de lesgever!")
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human="Ongeldige exception."
            ),
            readable_expected=expected.readable(),
            readable_actual="",
            messages=[staff_message, student_message]
        )

    return EvaluationResult(
        result=StatusMessage(
            enum=Status.CORRECT if expected.message == actual.message else
            Status.WRONG
        ),
        readable_expected=expected.readable(),
        readable_actual=actual.readable()
    )
