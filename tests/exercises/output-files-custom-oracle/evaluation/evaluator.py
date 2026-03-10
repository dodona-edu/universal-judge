import os
from evaluation_utils import EvaluationResult


def evaluate(context):
    file_path = os.path.join(context.execution_directory, "even.txt")
    if not os.path.isfile(file_path):
        return EvaluationResult(False, "The file even.txt should exist", f"The file even.txt was not found in {context.execution_directory}")

    with open(file_path, "r") as f:
        content = f.read().splitlines()

    # Check if all numbers from 1 to 10 are even
    try:
        numbers = [int(line.strip()) for line in content if line.strip()]
    except ValueError:
        return EvaluationResult(False, "The file even.txt should contain even numbers", f"The file even.txt contains non-numeric data: {content}")

    expected_numbers = {2, 4, 6, 8, 10}
    actual_numbers = set(numbers)

    if expected_numbers == actual_numbers:
        return EvaluationResult(True, str(expected_numbers), str(actual_numbers))
    else:
        return EvaluationResult(False, f"Expected {expected_numbers}, but got {actual_numbers}", str(actual_numbers))
