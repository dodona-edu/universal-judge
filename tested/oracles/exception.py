import logging
import traceback
from typing import Optional, Tuple

from tested.dodona import ExtendedMessage, Message, Permission, Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.languages.utils import convert_stacktrace_to_clickable_feedback
from tested.oracles.common import OracleConfig, OracleResult
from tested.parsing import get_converter
from tested.serialisation import ExceptionValue
from tested.testsuite import ExceptionOutputChannel, OutputChannel

_logger = logging.getLogger(__name__)


def try_as_exception(config: OracleConfig, value: str) -> ExceptionValue:
    actual = get_converter().loads(value, ExceptionValue)
    actual.stacktrace = config.bundle.lang_config.cleanup_stacktrace(actual.stacktrace)
    return actual


def try_as_readable_exception(
    config: OracleConfig, value: str
) -> Tuple[Optional[str], Optional[Message]]:
    # noinspection PyBroadException
    try:
        actual = get_converter().loads(value, ExceptionValue)
        actual.stacktrace = config.bundle.lang_config.cleanup_stacktrace(
            actual.stacktrace
        )
    except Exception:
        return None, None
    else:
        readable = actual.readable(omit_type=False)
        message = convert_stacktrace_to_clickable_feedback(
            config.bundle.lang_config, actual.stacktrace
        )
        return readable, message


def evaluate(
    config: OracleConfig, channel: OutputChannel, actual_str: str
) -> OracleResult:
    """
    Evaluate an exception.

    :param config: Not used.
    :param channel: The channel from the test suite.
    :param actual_str: The raw actual value of the execution.

    :return: An evaluation result.
    """
    assert isinstance(channel, ExceptionOutputChannel)
    assert channel.exception is not None

    expected = channel.exception
    language = config.bundle.global_config.dodona.programming_language
    readable_expected = expected.readable(language)

    if not actual_str:
        return OracleResult(
            result=StatusMessage(enum=Status.WRONG),
            readable_expected=readable_expected,
            readable_actual="",
            messages=[],
        )

    try:
        actual = try_as_exception(config, actual_str)
    except Exception as e:
        _logger.exception(e)
        staff_message = ExtendedMessage(
            description=get_i18n_string(
                "evaluators.exception.staff",
                actual=actual_str,
                exception=traceback.format_exc(),
            ),
            format="text",
            permission=Permission.STAFF,
        )
        student_message = get_i18n_string("evaluators.exception.student")
        return OracleResult(
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

    return OracleResult(
        result=StatusMessage(enum=status),
        readable_expected=readable_expected,
        readable_actual=actual.readable(
            omit_type=expected_type is None and status != Status.CORRECT
        ),
        messages=messages,
    )
