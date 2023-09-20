import functools

from tested.dodona import ExtendedMessage, Permission, Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.oracles.common import OracleConfig, OracleResult, try_outputs
from tested.oracles.exception import try_as_readable_exception
from tested.oracles.value import compare_values, try_as_readable_value
from tested.serialisation import Expression, NothingType, parse_value
from tested.testsuite import EmptyChannel, OutputChannel, Testcase


def evaluate_nothing_others(
    config: OracleConfig, actual: str, unexpected_status: Status = Status.WRONG
) -> OracleResult:
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
            human=get_i18n_string(f"oracles.nothing.{error}"),
        )
    else:
        result = StatusMessage(enum=Status.CORRECT)

    return OracleResult(
        result=result, readable_expected="", readable_actual=actual, messages=messages
    )


def evaluate_no_return(
    config: OracleConfig, actual: str, unexpected_status: Status = Status.WRONG
) -> OracleResult:
    expected = NothingType()

    try:
        if actual == "":
            # If there was an exception, we still get the empty string.
            value = NothingType()
        else:
            value = parse_value(actual)
    except Exception as e:
        raw_message = f"Could not parse {actual}, which caused {e} for get_values."
        message = ExtendedMessage(
            description=raw_message, format="text", permission=Permission.STAFF
        )
        return OracleResult(
            result=StatusMessage(enum=Status.INTERNAL_ERROR),
            readable_expected="",
            readable_actual=actual,
            messages=[message],
        )

    type_check, _, content_check = compare_values(config, value, expected)
    if type_check and content_check:
        return OracleResult(
            result=StatusMessage(enum=Status.CORRECT),
            readable_expected="",
            readable_actual="",
            messages=[],
        )
    else:
        error = "runtime" if unexpected_status == Status.RUNTIME_ERROR else "unexpected"
        result = StatusMessage(
            enum=unexpected_status,
            human=get_i18n_string(f"oracles.nothing.{error}"),
        )
        return OracleResult(
            result=result, readable_expected="", readable_actual=actual, messages=[]
        )


def evaluate(
    config: OracleConfig,
    channel: OutputChannel,
    actual: str,
    unexpected_status: Status = Status.WRONG,
    testcase: Testcase | None = None,  # Only used if there is an expression.
) -> OracleResult:
    assert isinstance(channel, EmptyChannel)
    if testcase and isinstance(testcase.input, Expression):
        # If we are evaluating the return value and the input is a statement, we don't
        # want any return value (e.g. void in Java).
        # On the other hand, if we have an expression, we actually expect a return
        # value of "nothing", meaning null or None or undefined.
        # In that last case, we actually need to check it.
        return evaluate_no_return(config, actual, unexpected_status)
    else:
        return evaluate_nothing_others(config, actual, unexpected_status)
