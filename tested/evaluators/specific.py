"""
Specific evaluator
"""
from typing import Optional

from .utils import cleanup_specific_programmed
from ..dodona import StatusMessage, Status, ExtendedMessage, Permission
from . import EvaluationResult, EvaluatorConfig
from ..serialisation import EvalResult
from ..testplan import OutputChannel, ExceptionOutputChannel
from ..testplan import SpecificEvaluator


def evaluate(config: EvaluatorConfig, channel: OutputChannel,
             actual: str) -> EvaluationResult:
    """
    Compare the result of a specific evaluator. This evaluator has no options.
    """
    assert isinstance(channel.evaluator, SpecificEvaluator)

    # Special support for no values to have a better error message.
    if actual == "":
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human="Ontbrekende uitvoer."
            ),
            readable_actual="",
            readable_expected="",
            messages=["Hier ontbreekt uitvoer."]
        )

    # Try parsing as the result.
    try:
        actual: EvalResult = EvalResult.parse_raw(actual)
    except (TypeError, ValueError) as e:
        staff_message = ExtendedMessage(
            description=f"Received invalid output for specific evaluation: "
                        f"{actual!r}. Either the testplan is invalid, the "
                        f"evaluation code has a bug or the student is trying to "
                        f"cheat: {e}",
            format="text",
            permission=Permission.STAFF
        )
        student_message = ("Er ging iets fout bij het beoordelen van de "
                           "oplossing. Meld dit aan de lesgever!")
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human="Fout bij beoordelen resultaat."
            ),
            readable_expected="",
            readable_actual="",
            messages=[staff_message, student_message]
        )

    actual = cleanup_specific_programmed(config, channel, actual)

    return EvaluationResult(
        result=StatusMessage(
            enum=actual.result
        ),
        readable_expected=actual.readable_expected,
        readable_actual=actual.readable_actual,
        messages=actual.messages
    )
