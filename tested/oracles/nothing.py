import functools

from tested.dodona import Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.oracles.common import OracleConfig, OracleResult, try_outputs
from tested.oracles.exception import try_as_readable_exception
from tested.oracles.value import try_as_readable_value
from tested.testsuite import EmptyChannel, OutputChannel


def evaluate(
    config: OracleConfig,
    channel: OutputChannel,
    actual: str,
    unexpected_status: Status = Status.WRONG,
) -> OracleResult:
    assert isinstance(channel, EmptyChannel)
    messages = []

    if actual:
        parsers = [
            functools.partial(try_as_readable_exception, config),
            functools.partial(try_as_readable_value, config.bundle),
        ]
        actual, msg = try_outputs(actual, parsers)
        if msg:
            messages.append(msg)
        error = "runtime" if unexpected_status == Status.RUNTIME_ERROR else "unexpected"
        result = StatusMessage(
            enum=unexpected_status,
            human=get_i18n_string(f"evaluators.nothing.{error}"),
        )
    else:
        result = StatusMessage(enum=Status.CORRECT)

    return OracleResult(
        result=result, readable_expected="", readable_actual=actual, messages=messages
    )
