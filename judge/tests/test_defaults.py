"""Test the judge using Python code."""
from pathlib import Path

from testplan import parse_test_plan

FOLDER = Path(__file__).parent / "testplan"
FULL = FOLDER / "full.json"
SHORT = FOLDER / "short.json"


def test_correct_defaults():
    """Tests that the testplan uses the correct defaults if values are omitted."""
    with open(FULL, 'r') as f:
        full = parse_test_plan(f.read())

    with open(SHORT, 'r') as f:
        short = parse_test_plan(f.read())

    assert full == short

