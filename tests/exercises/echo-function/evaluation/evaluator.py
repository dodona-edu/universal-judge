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


def evaluate_permissions(actual):
    return EvaluationResult(actual == "correct", "correct", actual, [
        Message("for educators", permission="educator"),
        Message("for everyone"),
    ])


def evaluate_unknown_permission(actual):
    return EvaluationResult(actual == "correct", "correct", actual, [
        Message("secret for teachers", permission="teacher"),
    ])


def evaluate_staff_permission(actual):
    return EvaluationResult(actual == "correct", "correct", actual, [
        Message("secret for teachers", permission="staff"),
    ])


def evaluate_sys_without_import(_context):
    return EvaluationResult(sys.modules["evaluation_utils"].EvaluationResult is EvaluationResult)


def evaluate_set_of_arrays(context):
    return EvaluationResult(context.actual == {(1, 2), (3, 4)})
