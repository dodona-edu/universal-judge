# noinspection PyUnresolvedReferences
from evaluation_utils import EvaluationResult, Message


def evaluate(actual):
    correct = actual == "correct"
    return EvaluationResult(correct, "correct", actual, [Message("Hallo")])


def evaluate_value(expected, actual):
    return EvaluationResult(expected == actual, expected, actual, [Message("Hallo")])


def evaluate_value_dsl(expected, actual):
    return EvaluationResult(
        result=expected == actual,
        messages=[Message("Hallo")],
        dsl_expected="{5, 5}",
        dsl_actual="{4, 4}"
    )
