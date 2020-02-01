import evaluation_utils


def evaluate(expected, actual, arguments):
    evaluation_utils.evaluated(
        False,
        str(expected),
        str(actual),
        ["Hallo custom!", f"Actual is {actual}", f"Expected is {expected}"])
