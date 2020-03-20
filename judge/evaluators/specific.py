"""
Specific evaluator
"""
from dodona import StatusMessage, Status, ExtendedMessage, Permission
from evaluators import EvaluationResult
from serialisation import SpecificResult
from testplan.channels import OutputChannel
from testplan.evaluators import SpecificEvaluator


def evaluate(_, channel: OutputChannel, actual: str) -> EvaluationResult:
    """
    Compare the result of a specific evaluator. This evaluator has no options.
    """
    assert isinstance(channel.evaluator, SpecificEvaluator)

    # There is no output.
    if not actual:
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human="Ontbrekende uitvoer."
            ),
            readable_expected="",
            readable_actual="",
            messages=[

                "Er ging iets verkeerd bij het beoordelen van de oplossing. Meld "
                "dit aan de lesgever!"
            ]
        )

    # Try parsing as the result.
    try:
        actual: SpecificResult = SpecificResult.__pydantic_model__.parse_raw(actual)
    except (TypeError, ValueError) as e:
        staff_message = ExtendedMessage(
            description=f"Received invalid output for programmed evaluation: "
                        f"{actual!r}. Either the testplan is invalid, the "
                        f"evaluation code has a bug or the student is trying to "
                        f"cheat: {e}",
            format="text",
            permission=Permission.STAFF
        ),
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

    return EvaluationResult(
        result=StatusMessage(
            enum=Status.CORRECT if actual.result else Status.WRONG
        ),
        readable_expected=actual.readable_expected,
        readable_actual=actual.readable_actual,
        messages=actual.messages
    )
