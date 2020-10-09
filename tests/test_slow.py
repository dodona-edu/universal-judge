"""
Tests where full exercises are run. These are inherently slower.
"""
from pathlib import Path
from typing import Tuple

import pytest

from tests.manual_utils import configuration, execute_config, assert_valid_output


@pytest.mark.slow
@pytest.mark.parametrize("lang", ["python", "java", "kotlin"])
def test_full_isbn(lang: str, tmp_path: Path, pytestconfig):
    config_ = {
        "options": {
            "parallel": True
        }
    }
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "full.tson", "solution", options=config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 150
    assert updates.find_status_enum() == ["correct"] * 100


@pytest.mark.slow
@pytest.mark.parametrize("lang", ["haskell", "java", "python", "runhaskell", "kotlin"])
def test_full_isbn_list(lang: str, tmp_path: Path, pytestconfig):
    config_ = {
        "options": {
            "parallel": True
        }
    }
    conf = configuration(pytestconfig, "isbn-list", lang, tmp_path, "plan.tson", "solution", options=config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 150
    assert updates.find_status_enum() == ["correct"] * 100


@pytest.mark.slow
@pytest.mark.parametrize("lang", ["java", "python", "kotlin"])
def test_full_lotto(lang: str, tmp_path: Path, pytestconfig):
    config_ = {
        "options": {
            "parallel": True
        }
    }
    conf = configuration(pytestconfig, "lotto", lang, tmp_path, "plan.tson", "correct", options=config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 45
    assert updates.find_status_enum() == ["correct"] * 45


@pytest.mark.flaky
@pytest.mark.parametrize("language_and_time", [("python", 2), ("java", 5), ("haskell", 20), ("c", 3),
                                               ("runhaskell", 15), ("kotlin", 5)])
def test_timeout(language_and_time: Tuple[str, int], tmp_path: Path, pytestconfig):
    config_ = {
        "time_limit": language_and_time[1]  # seconds
    }
    conf = configuration(pytestconfig, "echo", language_and_time[0], tmp_path, "full.tson", "correct", config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 50
    status = updates.find_status_enum()
    correct = [x for x in status if x == "correct"]
    exceeded = [x for x in status if x == "time limit exceeded"]
    wrong = [x for x in status if x == "wrong"]
    # We should have at least one good result.
    assert len(correct) >= 1
    assert len(wrong) <= 2
    assert len(exceeded) >= 1
    # Once for every status, plus one escalation, plus one judgement-close
    assert len(wrong + correct + exceeded) == 50 + 1 + 1


@pytest.mark.slow
@pytest.mark.parametrize("lang", ["haskell", "java", "python", "c", "javascript", "kotlin"])
def test_full_echo(lang: str, tmp_path: Path, pytestconfig):
    config_ = {
        "options": {
            "parallel": True
        }
    }
    conf = configuration(pytestconfig, "echo", lang, tmp_path, "full.tson", "correct", options=config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 50
    assert updates.find_status_enum() == ["correct"] * 50
