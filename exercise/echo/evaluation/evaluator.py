from evaluation_utils import EvaluationResult


def evaluate_correct(expected, actual, arguments):
    return EvaluationResult(expected.strip() == actual.strip())


def evaluate_wrong(expected, actual, arguments):
    return EvaluationResult(False)