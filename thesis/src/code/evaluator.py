from evaluation_utils import EvaluationResult, Message

def evaluate(expected, actual, arguments):
    # Controleer of de gekregen waarde overeenkomt.
    correct = expected == actual
    messages = []
    if not correct:
        messages.append(Message("Hallo, dit is niet juist!"))
    # We geven geen verwachte waarde mee; TESTed neemt de waarde uit het testplan.
    return EvaluationResult(correct, None, actual, messages)
