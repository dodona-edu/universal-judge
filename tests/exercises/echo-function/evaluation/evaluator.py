from evaluation_utils import EvaluationResult, Message


def evaluate(actual):
    correct = actual == "correct"
    return EvaluationResult(correct, "correct", actual, [Message("Hallo")])


def evaluate_value(expected, actual, args):
    return EvaluationResult(expected == actual, expected, actual, [Message("Hallo")])
