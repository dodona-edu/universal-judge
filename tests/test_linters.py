from pathlib import Path

import pytest

from tests.manual_utils import configuration, execute_config, assert_valid_output, mark_haskell


@pytest.mark.linter
def test_cppcheck(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", "c", tmp_path, "one.tson", "correct-cppcheck", {
        "options": {
            "linter": {
                "c": True
            }
        }
    })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
def test_checkstyle(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "counter", "java", tmp_path, "plan.yaml", "solution-checkstyle", {
        "options": {
            "linter": {
                "java": True
            }
        }
    })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
def test_eslint(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "counter", "javascript", tmp_path, "plan.yaml", "solution-eslint", {
        "options": {
            "linter": {
                "javascript": True
            }
        }
    })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
@mark_haskell
@pytest.mark.parametrize("language", ["haskell", "runhaskell"])
def test_hlint(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct_io", {
        "options": {
            "linter": {
                language: True
            }
        }
    })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
@pytest.mark.ktlint
def test_ktlint(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "counter", "kotlin", tmp_path, "plan.yaml", "solution", {
        "options": {
            "linter": {
                "kotlin": True
            }
        }
    })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
def test_pylint(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "counter", "python", tmp_path, "plan.yaml", "solution-pylint", {
        "options": {
            "linter": {
                "python": True
            }
        }
    })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0
