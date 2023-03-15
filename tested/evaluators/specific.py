"""
Specific evaluator
"""

from tested.dodona import ExtendedMessage, Permission, Status, StatusMessage
from tested.evaluators import EvaluationResult, EvaluatorConfig
from tested.evaluators.utils import cleanup_specific_programmed
from tested.internationalization import get_i18n_string
from tested.serialisation import EvalResult
from tested.testsuite import OutputChannel, SpecificEvaluator


def evaluate(
    config: EvaluatorConfig, channel: OutputChannel, actual: str
) -> EvaluationResult:
    """
    Compare the result of a specific evaluator. This evaluator has no options.
    """
    assert isinstance(channel.evaluator, SpecificEvaluator)

    # Special support for no values to have a better error message.
    if actual == "":
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human=get_i18n_string("evaluators.specific.missing.status"),
            ),
            readable_actual="",
            readable_expected="",
            messages=[get_i18n_string("evaluators.specific.missing.message")],
        )

    # Try parsing as the result.
    try:
        actual: EvalResult = EvalResult.parse_raw(actual)
    except (TypeError, ValueError) as e:
        staff_message = ExtendedMessage(
            description=get_i18n_string(
                "evaluators.specific.staff", actual=actual, e=e
            ),
            format="text",
            permission=Permission.STAFF,
        )
        student_message = get_i18n_string("evaluators.specific.student.default")
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human=get_i18n_string("evaluators.specific.status"),
            ),
            readable_expected="",
            readable_actual="",
            messages=[staff_message, student_message],
        )

    actual = cleanup_specific_programmed(config, channel, actual)

    return EvaluationResult(
        result=StatusMessage(enum=actual.result),
        readable_expected=actual.readable_expected,
        readable_actual=actual.readable_actual,
        messages=actual.messages,
    )
