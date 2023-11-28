# noinspection PyUnresolvedReferences
from evaluation_utils import EvaluationResult


def evaluate_correct(context):
    return EvaluationResult(context["expected"].strip() == context["actual"].strip())


def evaluate_wrong(_context):
    return EvaluationResult(False)
