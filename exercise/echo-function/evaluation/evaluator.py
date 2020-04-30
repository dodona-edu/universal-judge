from evaluation_utils import EvaluationResult


def evaluate(actual):
    correct = actual == "correct"
    return EvaluationResult(correct, "correct", actual, ["Hallo"])
