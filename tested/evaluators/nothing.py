"""
RawEvaluator for channels without output.
"""
import functools
from typing import Optional

from tested.dodona import StatusMessage, Status
from . import EvaluationResult, try_outputs, exception, value, EvaluatorConfig
from ..testplan import EmptyChannel


def evaluate(config: EvaluatorConfig, channel: EmptyChannel,
             actual: str) -> EvaluationResult:
    assert isinstance(channel, EmptyChannel)
    messages = []

    if actual:
        parsers = [
            functools.partial(exception.try_as_readable_exception, config),
            functools.partial(value.try_as_readable_value, config.bundle)
        ]
        actual, msg = try_outputs(actual, parsers)
        if msg:
            messages.append(msg)
        result = StatusMessage(
            enum=Status.WRONG,
            human="Onverwachte uitvoer."
        )
    else:
        result = StatusMessage(enum=Status.CORRECT)

    return EvaluationResult(
        result=result,
        readable_expected="",
        readable_actual=actual,
        messages=messages
    )
