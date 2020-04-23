from evaluation_utils import EvaluationResult

def evaluate(actual):
    correct = isinstance(actual, int)
    messages = []
    if not correct:
        messages.append("Dit is geen int!")
    return EvaluationResult(correct, str(5), str(actual), messages)
