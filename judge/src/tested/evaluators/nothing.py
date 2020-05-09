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

    if actual:
        parsers = [
            exception.try_as_readable_exception,
            functools.partial(value.try_as_readable_value, config.bundle)
        ]
        actual = try_outputs(actual, parsers)
        result = StatusMessage(
            enum=Status.WRONG,
            human="Onverwachte uitvoer."
        )
    else:
        result = StatusMessage(enum=Status.CORRECT)

    return EvaluationResult(
        result=result,
        readable_expected="",
        readable_actual=actual
    )
