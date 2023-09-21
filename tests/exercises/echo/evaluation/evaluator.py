# noinspection PyUnresolvedReferences
from evaluation_utils import EvaluationResult


def evaluate_correct(expected, actual):
    return EvaluationResult(expected.strip() == actual.strip())


def evaluate_wrong(_expected, _actual):
    return EvaluationResult(False)
