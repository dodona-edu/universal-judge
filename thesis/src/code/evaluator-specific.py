from evaluation_utils import EvaluationResult, Message

def evaluate(actual):
    correct = isinstance(actual, int)
    messages = []
    if not correct:
        messages.append(Message("Dit is geen int!"))
    return EvaluationResult(correct, str(5), str(actual), messages)
