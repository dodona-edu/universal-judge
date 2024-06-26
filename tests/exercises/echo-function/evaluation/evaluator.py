# noinspection PyUnresolvedReferences
from evaluation_utils import EvaluationResult, Message


def evaluate(actual):
    correct = actual == "correct"
    return EvaluationResult(correct, "correct", actual, [Message("Hallo")])


def evaluate_value(context):
    return EvaluationResult(context.expected == context.actual, context.expected, context.actual, [Message("Hallo")])


def evaluate_value_dsl(context):
    return EvaluationResult(
        result=context.expected == context.actual,
        messages=[Message("Hallo")],
        dsl_expected="{5, 5}",
        dsl_actual="{4, 4}"
    )


def evaluate_runtime_crash(context):
    return len(context) / 0


def evaluate_sum(actual, the_sum):
    correct = the_sum == 10
    return EvaluationResult(correct, "correct", actual, [Message("Hallo")])

