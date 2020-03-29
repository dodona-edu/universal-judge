"""
RawEvaluator for ignored channels.
"""
import functools

from ..dodona import StatusMessage, Status
from . import EvaluationResult, exception, value, try_outputs, EvaluatorConfig
from ..testplan import IgnoredChannel


def evaluate(config: EvaluatorConfig, channel: IgnoredChannel, actual: str,
             _ignored: Status) -> EvaluationResult:
    assert channel == IgnoredChannel.IGNORED

    # If there is something in the channel, try parsing it as
    # an exception or a value.
    parsers = [
        exception.try_as_readable_exception,
        functools.partial(value.try_as_readable_value, config.bundle)
    ]
    actual = try_outputs(actual, parsers)

    return EvaluationResult(
        result=StatusMessage(enum=Status.CORRECT),
        readable_expected="",
        readable_actual=actual
    )
