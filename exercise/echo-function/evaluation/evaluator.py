from evaluation_utils import EvaluationResult


def evaluate(actual):
    correct = actual == "correct"
    return EvaluationResult(correct, "correct", actual, ["Hallo"])


def evaluate_value(expected, actual, messages):
    return EvaluationResult(expected == actual, expected, actual, ["Hallo"])
