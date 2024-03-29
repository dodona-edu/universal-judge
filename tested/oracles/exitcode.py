import logging

from tested.dodona import Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.oracles.common import OracleConfig, OracleResult
from tested.testsuite import ExitCodeOutputChannel, OutputChannel

logger = logging.getLogger(__name__)


def _as_int(value: str) -> int | None:
    try:
        return int(value)
    except (ValueError, TypeError):
        return None


def evaluate(_config: OracleConfig, channel: OutputChannel, value: str) -> OracleResult:
    assert isinstance(channel, ExitCodeOutputChannel)
    exit_code = _as_int(value)

    if exit_code is None:
        return OracleResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human=get_i18n_string(
                    "oracles.exitcode.status.invalid", exit_code=value
                ),
            ),
            readable_expected=str(channel.value),
            readable_actual=str(value),
            messages=[
                get_i18n_string("oracles.exitcode.status.message", exit_code=value)
            ],
        )

    expected_exit_code = channel.value

    if expected_exit_code != exit_code:
        status = StatusMessage(
            enum=Status.WRONG, human=get_i18n_string("oracles.exitcode.status.wrong")
        )
    else:
        status = StatusMessage(enum=Status.CORRECT)

    return OracleResult(
        result=status,
        readable_expected=str(expected_exit_code),
        readable_actual=str(exit_code),
    )
