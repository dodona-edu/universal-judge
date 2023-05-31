"""
Exception evaluator.
"""
import logging
from typing import Optional, Tuple

from pydantic import BaseModel

from tested.dodona import ExtendedMessage, Permission, Status, StatusMessage
from tested.evaluators.common import EvaluationResult, EvaluatorConfig
from tested.internationalization import get_i18n_string
from tested.languages.utils import convert_stacktrace_to_clickable_feedback
from tested.serialisation import ExceptionValue
from tested.testsuite import ExceptionOutputChannel

logger = logging.getLogger(__name__)


class _ExceptionValue(BaseModel):
    __root__: ExceptionValue


def try_as_exception(config: EvaluatorConfig, value: str) -> ExceptionValue:
    actual = _ExceptionValue.parse_raw(value).__root__
    actual.stacktrace = config.bundle.lang_config.cleanup_stacktrace(actual.stacktrace)
    return actual


def try_as_readable_exception(
    config: EvaluatorConfig, value: str
) -> Tuple[Optional[str], Optional[ExtendedMessage]]:
    try:
        actual = _ExceptionValue.parse_raw(value).__root__
        actual.stacktrace = config.bundle.lang_config.cleanup_stacktrace(
            actual.stacktrace
        )
    except (TypeError, ValueError):
        return None, None
    else:
        readable = actual.readable(omit_type=False)
        message = convert_stacktrace_to_clickable_feedback(
            config.bundle.lang_config, actual.stacktrace
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
        actual = try_as_exception(config, actual)
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

    expected_type = expected.get_type(language)

    # If there is type information, check it.
    if expected_type:
        type_is_ok = expected_type == actual.type
    else:
        type_is_ok = True
    message_is_ok = expected.message == actual.message or expected.message is None

    status = Status.CORRECT if (type_is_ok and message_is_ok) else Status.WRONG

    messages = []
    # Append the stacktrace as an HTML message if possible.
    # To keep things clean, we only do this if the test is incorrect.

    cleaned_stacktrace = convert_stacktrace_to_clickable_feedback(
        config.bundle.lang_config, actual.stacktrace
    )
    if cleaned_stacktrace and status != Status.CORRECT:
        messages.append(cleaned_stacktrace)

    # If the result is correct, substitute the expected value with the correct type.
    if status == Status.CORRECT:
        readable_expected = actual.readable(omit_type=False)

    if status != Status.CORRECT:
        # Add additional messages if present.
        for message in actual.additional_message_keys:
            messages.append(
                get_i18n_string(message, actual_type=(actual.type or actual.message))
            )

    return EvaluationResult(
        result=StatusMessage(enum=status),
        readable_expected=readable_expected,
        readable_actual=actual.readable(
            omit_type=expected_type is None and status != Status.CORRECT
        ),
        messages=messages,
    )
