"""
Exception evaluator.
"""
import logging
from typing import Optional, Tuple

from pydantic import BaseModel

from tested.dodona import ExtendedMessage, Permission, Status, StatusMessage
from tested.evaluators.common import EvaluationResult, EvaluatorConfig
from tested.internationalization import get_i18n_string
from tested.serialisation import ExceptionValue
from tested.testsuite import ExceptionOutputChannel
from tested.utils import Either

logger = logging.getLogger(__name__)


class _ExceptionValue(BaseModel):
    __root__: ExceptionValue


def try_as_exception(config: EvaluatorConfig, value: str) -> Either[ExceptionValue]:
    try:
        actual = _ExceptionValue.parse_raw(value).__root__
        actual = config.bundle.lang_config.exception_output(actual)
        return Either(actual)
    except (TypeError, ValueError) as e:
        return Either(e)


def try_as_readable_exception(
    config: EvaluatorConfig, value: str
) -> Tuple[Optional[str], Optional[ExtendedMessage]]:
    try:
        actual = _ExceptionValue.parse_raw(value).__root__
        actual = config.bundle.lang_config.exception_output(actual)
    except (TypeError, ValueError):
        return None, None
    else:
        readable = actual.readable()
        message = config.bundle.lang_config.clean_stacktrace_to_message(
            actual.stacktrace
        )
        return readable, message


def evaluate(
    config: EvaluatorConfig, channel: ExceptionOutputChannel, actual: str
) -> EvaluationResult:
    """
    Evaluate an exception.

    :param config: Not used.
    :param channel: The channel from the test suite.
    :param actual: The raw actual value of the execution.

    :return: An evaluation result.
    """
    assert isinstance(channel, ExceptionOutputChannel)
    assert channel.exception is not None

    expected = channel.exception
    language = config.bundle.global_config.dodona.programming_language
    readable_expected = expected.readable(language)

    if not actual:
        return EvaluationResult(
            result=StatusMessage(enum=Status.WRONG),
            readable_expected=readable_expected,
            readable_actual="",
            messages=[],
        )

    try:
        actual = try_as_exception(config, actual).get()
    except (TypeError, ValueError) as e:
        staff_message = ExtendedMessage(
            description=get_i18n_string(
                "evaluators.exception.staff", actual=actual, exception=e
            ),
            format="text",
            permission=Permission.STAFF,
        )
        student_message = get_i18n_string("evaluators.exception.student")
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human=get_i18n_string("evaluators.exception.status"),
            ),
            readable_expected=readable_expected,
            readable_actual="",
            messages=[staff_message, student_message],
        )

    messages = []
    # Append the stacktrace as an HTML message if possible.
    # We do this even if it is correct.
    cleaned_stacktrace = config.bundle.lang_config.clean_stacktrace_to_message(
        actual.stacktrace
    )
    if cleaned_stacktrace:
        messages.append(cleaned_stacktrace)

    # If there is type information, check it.
    if expected_type := expected.get_type(language):
        type_is_ok = expected_type == actual.type
    else:
        type_is_ok = True
    message_is_ok = expected.message == actual.message or expected.message is None

    status = Status.CORRECT if (type_is_ok and message_is_ok) else Status.WRONG

    # If the result is correct, substitute the expected value with the correct type.
    if status == Status.CORRECT:
        readable_expected = actual.readable()

    return EvaluationResult(
        result=StatusMessage(enum=status),
        readable_expected=readable_expected,
        readable_actual=actual.readable(),
        messages=messages,
    )
