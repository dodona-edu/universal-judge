"""Test the judge using Python code."""
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


def test_wrong_main():
    code = FOLDER / "wrong_main.py"
    validate_output(code, PLAN)
    validate_result(code, PLAN, FOLDER / "wrong_main.result")


def test_wrong_second():
    code = FOLDER / "wrong_second.py"
    validate_output(code, PLAN)
    validate_result(code, PLAN, FOLDER / "wrong_second.result")
