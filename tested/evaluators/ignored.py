"""
RawEvaluator for ignored channels.
"""
import functools

from tested.dodona import Status, StatusMessage
from tested.evaluators.common import EvaluationResult, EvaluatorConfig, try_outputs
from tested.evaluators.exception import try_as_readable_exception
from tested.evaluators.value import try_as_readable_value
from tested.testsuite import IgnoredChannel, OutputChannel


def evaluate(
    config: EvaluatorConfig, channel: OutputChannel, actual: str
) -> EvaluationResult:
    assert isinstance(channel, IgnoredChannel)

    # If there is something in the channel, try parsing it as
    # an exception or a value.
    parsers = [
        functools.partial(try_as_readable_exception, config),
        functools.partial(try_as_readable_value, config.bundle),
    ]
    actual, msg = try_outputs(actual, parsers)
    messages = [msg] if msg else []

    return EvaluationResult(
        result=StatusMessage(enum=Status.CORRECT),
        readable_expected="",
        readable_actual=actual,
        messages=messages,
    )
