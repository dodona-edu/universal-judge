import re
import ast
# noinspection PyUnresolvedReferences
from evaluation_utils import EvaluationResult, Message


def listing(numbers):
    if len(numbers) == 1:
        return str(numbers[0])
    else:
        return f"{', '.join(str(x) for x in numbers[:-1])} en {numbers[-1]}"


def valid_lottery_numbers(number_str, count=6, maximum=42):
    if not isinstance(number_str, str):
        return False, "lottogetallen moet een string zijn"

    if not re.match("^(-?(0|-?[1-9][0-9]*) - )*(0|-?[1-9][0-9]*)$", number_str):
        return False, "lottogetallen worden niet in correct formaat teruggegeven"

    nrs = [int(x) for x in number_str.split(" - ")]
    if len(nrs) != count:
        return False, f"verwachtte {count} in plaats van {len(nrs)} lottogetallen"

    if wrong := [number for number in nrs if number > maximum]:
        return (False, "volgende lottogetallen zijn groter dan de maximale waarde "
                       f"{maximum}: {listing(wrong)}")

    if wrong := [number for number in nrs if number < 1]:
        return (False, "volgende lottogetallen zijn kleiner dan de minimale "
                       f"waarde 1: {listing(wrong)}")

    duplicates = {number for number in nrs if nrs.count(number) > 1}
    if wrong := sorted(duplicates):
        return (False, "volgende lottogetallen komen meer dan één keer "
                       f"voor: {listing(wrong)}")

    if list(sorted(nrs)) != nrs:
        return False, "lottogetallen worden niet in stijgende volgorde opgelijst"

    return True, None


def evaluate(context, count, maximum):
    expected = context.expected
    actual = context.actual
    valid, message = valid_lottery_numbers(actual, count, maximum)
    messages = [Message(message)] if message else []
    if valid:
        expected = actual
    return EvaluationResult(valid, expected, actual, messages)


def check_for_node(search, context, count, maximum):
    assert context.programming_language == "python", "This exercise only supports Python"
    # Check if the submission uses a while loop.
    with open(context.submission_path, "r") as submission_file:
        submission = submission_file.read()

    # This has no error handling, so it is not ready for production.
    nodes = ast.walk(ast.parse(submission))
    has_while = any(isinstance(node, search) for node in nodes)
    messages = []
    if not has_while:
        messages.append("Your code does not use a while loop, which is mandatory.")
    eval_result = evaluate(context, count, maximum)

    return EvaluationResult(
        eval_result.result and has_while,
        eval_result.readable_expected,
        eval_result.readable_actual,
        eval_result.messages + messages
    )


def check_for_while(context, count, maximum):
    return check_for_node(ast.While, context, count, maximum)


def check_for_for(context, count, maximum):
    return check_for_node(ast.For, context, count, maximum)
