"""
Exception evaluator.
"""
from tested.dodona import StatusMessage, Status, ExtendedMessage, Permission
from evaluators import EvaluationResult
from tested.serialisation import ExceptionValue
from testplan.channels import ExceptionOutputChannel
from tested.utils import Either


def try_as_exception(value: str) -> Either[ExceptionValue]:
    try:
        actual = ExceptionValue.__pydantic_model__.parse_raw(value)
        return Either(actual)
    except (TypeError, ValueError) as e:
        return Either(e)


def evaluate(_, channel: ExceptionOutputChannel, actual: str) -> EvaluationResult:
    assert isinstance(channel, ExceptionOutputChannel)
    assert channel.exception is not None

    expected = channel.exception

    try:
        actual = try_as_exception(actual).get()
    except (TypeError, ValueError) as e:
        staff_message = ExtendedMessage(
            description=f"Expected value exception, but received {actual}, which "
                        f"caused an exception while parsing: {e}",
            format="text",
            permission=Permission.STAFF
        )
        student_message = ("Dodona verstond de exception niet. Meld deze fout aan "
                           "de lesgever!")
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human="Ongeldige exception."
            ),
            readable_expected=expected.readable(),
            readable_actual="",
            messages=[staff_message, student_message]
        )

    return EvaluationResult(
        result=StatusMessage(
            enum=Status.CORRECT if expected.message == actual.message else
            Status.WRONG
        ),
        readable_expected=expected.readable(),
        readable_actual=actual.readable()
    )
