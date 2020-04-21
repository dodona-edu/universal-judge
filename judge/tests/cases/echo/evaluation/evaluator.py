from evaluation_utils import EvaluationResult


def evaluate(expected, actual, arguments):
    return EvaluationResult(expected.strip() == actual.strip())
