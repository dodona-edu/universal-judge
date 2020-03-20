"""
Evaluator for ignored channels.
"""

from dodona import StatusMessage, Status
from evaluators import EvaluationResult, exception, value, try_outputs
from testplan.channels import IgnoredOutputChannel


def evaluate(_, channel: IgnoredOutputChannel, actual: str) -> EvaluationResult:
    assert channel == IgnoredOutputChannel.IGNORED

    # If there is something in the channel, try parsing it as
    # an exception or a value.
    actual = try_outputs(actual, [exception.try_as_exception, value.try_as_value])

    return EvaluationResult(
        result=StatusMessage(enum=Status.CORRECT),
        readable_expected="",
        readable_actual=actual
    )
