"""
Evaluate the result of a language-specific oracle.
"""
import logging
import traceback

from tested.dodona import ExtendedMessage, Permission, Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.oracles.common import (
    OracleConfig,
    OracleResult,
    cleanup_specific_programmed,
)
from tested.parsing import get_converter
from tested.serialisation import BooleanEvalResult
from tested.testsuite import LanguageSpecificOracle, OracleOutputChannel, OutputChannel

_logger = logging.getLogger(__name__)


def evaluate(
    config: OracleConfig, channel: OutputChannel, actual_str: str
) -> OracleResult:
    """
    Compare the result of a specific oracle. This oracle has no options.
    """
    assert isinstance(channel, OracleOutputChannel)
    assert isinstance(channel.oracle, LanguageSpecificOracle)

    # Special support for no values to have a better error message.
    if actual_str == "":
        return OracleResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human=get_i18n_string("evaluators.specific.missing.status"),
            ),
            readable_actual="",
            readable_expected="",
            messages=[get_i18n_string("evaluators.specific.missing.message")],
        )

    try:
        actual = get_converter().loads(actual_str, BooleanEvalResult).as_eval_result()
    except Exception as e:
        _logger.exception(e)
        staff_message = ExtendedMessage(
            description=get_i18n_string(
                "evaluators.specific.staff", actual=actual_str, e=traceback.format_exc()
            ),
            format="text",
            permission=Permission.STAFF,
        )
        student_message = get_i18n_string("evaluators.specific.student.default")
        return OracleResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human=get_i18n_string("evaluators.specific.status"),
            ),
            readable_expected="",
            readable_actual="",
            messages=[staff_message, student_message],
        )

    actual = cleanup_specific_programmed(config, channel, actual)

    return OracleResult(
        result=StatusMessage(enum=actual.result),
        readable_expected=actual.readable_expected or "",
        readable_actual=actual.readable_actual or "",
        messages=actual.messages,
    )
