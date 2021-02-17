"""
RawEvaluator for ignored channels.
"""
import functools

from . import EvaluationResult, exception, value, try_outputs, EvaluatorConfig
from ..dodona import StatusMessage, Status
from ..testplan import IgnoredChannel


def evaluate(config: EvaluatorConfig, channel: IgnoredChannel,
             actual: str) -> EvaluationResult:
    assert isinstance(channel, IgnoredChannel)

    # If there is something in the channel, try parsing it as
    # an exception or a value.
    parsers = [
        functools.partial(exception.try_as_readable_exception, config),
        functools.partial(value.try_as_readable_value, config.bundle)
    ]
    actual, msg = try_outputs(actual, parsers)
    messages = [msg] if msg else []

    return EvaluationResult(
        result=StatusMessage(enum=Status.CORRECT),
        readable_expected="",
        readable_actual=actual,
        messages=messages
    )
