"""Test the judge using the Python runner."""
from pathlib import Path

from tests.common import validate_result, validate_output

FOLDER = Path(__file__).parent / "python"
PLAN = FOLDER / "simple.json"


def test_everything_ok():
    code = FOLDER / "everything_ok.py"
    validate_output(code, PLAN)
    validate_result(code, PLAN, FOLDER / "everything_ok.result")


def test_deliberate_stop():
    code = FOLDER / "deliberate_stop.py"
    validate_output(code, PLAN)
    validate_result(code, PLAN, FOLDER / "deliberate_stop.result")


def test_not_enough_output():
    code = FOLDER / "not_enough_output.py"
    validate_output(code, PLAN)
    validate_result(code, PLAN, FOLDER / "not_enough_output.result")


def test_runtime_error():
    code = FOLDER / "runtime_error.py"
    validate_output(code, PLAN)
    # TODO: problem with paths and stacktraces.
    # validate_result(code, PLAN, FOLDER / "runtime_error.result")


def test_syntax_error():
    code = FOLDER / "syntax_error.py"
    validate_output(code, PLAN)
    # TODO: problem with paths and stacktraces.
    # validate_result(code, PLAN, FOLDER / "syntax_error.result")


def test_too_much_input_requested():
    code = FOLDER / "too_much_input_requested.py"
    validate_output(code, PLAN)
    # TODO: problem with paths and stacktraces.
    # validate_result(code, PLAN, FOLDER / "too_much_input_requested.result")


def test_wrong_main():
    code = FOLDER / "wrong_main.py"
    validate_output(code, PLAN)
    validate_result(code, PLAN, FOLDER / "wrong_main.result")


def test_wrong_second():
    code = FOLDER / "wrong_second.py"
    validate_output(code, PLAN)
    validate_result(code, PLAN, FOLDER / "wrong_second.result")
