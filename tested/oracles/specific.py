"""
Evaluate the result of a language-specific oracle.
"""

import logging
import traceback

from tested.dodona import ExtendedMessage, Permission, Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.oracles.common import BooleanEvalResult, OracleConfig, OracleResult
from tested.parsing import get_converter
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
                human=get_i18n_string("oracles.specific.missing.status"),
            ),
            readable_actual="",
            readable_expected="",
            messages=[get_i18n_string("oracles.specific.missing.message")],
        )

    try:
        actual = get_converter().loads(actual_str, BooleanEvalResult)
    except Exception as e:
        _logger.exception(e)
        staff_message = ExtendedMessage(
            description=get_i18n_string(
                "oracles.specific.staff", actual=actual_str, e=traceback.format_exc()
            ),
            format="text",
            permission=Permission.STAFF,
        )
        student_message = get_i18n_string("oracles.specific.student.default")
        return OracleResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human=get_i18n_string("oracles.specific.status"),
            ),
            readable_expected="",
            readable_actual="",
            messages=[staff_message, student_message],
        )

    return actual.to_oracle_result(config.bundle, channel, "", "")
