"""
RawEvaluator for channels without output.
"""
import functools

from tested.dodona import Status, StatusMessage
from tested.evaluators.common import EvaluationResult, EvaluatorConfig, try_outputs
from tested.evaluators.exception import try_as_readable_exception
from tested.evaluators.value import try_as_readable_value
from tested.internationalization import get_i18n_string
from tested.testsuite import EmptyChannel, OutputChannel


def evaluate(
    config: EvaluatorConfig,
    channel: OutputChannel,
    actual: str,
    unexpected_status: Status = Status.WRONG,
) -> EvaluationResult:
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
        result = StatusMessage(
            enum=unexpected_status,
            human=get_i18n_string(
                "evaluators.nothing."
                + (
                    "runtime"
                    if unexpected_status == Status.RUNTIME_ERROR
                    else "unexpected"
                )
            ),
        )
    else:
        result = StatusMessage(enum=Status.CORRECT)

    return EvaluationResult(
        result=result, readable_expected="", readable_actual=actual, messages=messages
    )
