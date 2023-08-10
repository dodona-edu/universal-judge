import functools

from tested.dodona import Status, StatusMessage
from tested.oracles.common import OracleConfig, OracleResult, try_outputs
from tested.oracles.exception import try_as_readable_exception
from tested.oracles.value import try_as_readable_value
from tested.testsuite import IgnoredChannel, OutputChannel


def evaluate(config: OracleConfig, channel: OutputChannel, actual: str) -> OracleResult:
    assert isinstance(channel, IgnoredChannel)

    # If there is something in the channel, try parsing it as
    # an exception or a value.
    parsers = [
        functools.partial(try_as_readable_exception, config),
        functools.partial(try_as_readable_value, config.bundle),
    ]
    actual, msg = try_outputs(actual, parsers)
    messages = [msg] if msg else []

    return OracleResult(
        result=StatusMessage(enum=Status.CORRECT),
        readable_expected="",
        readable_actual=actual,
        messages=messages,
    )
