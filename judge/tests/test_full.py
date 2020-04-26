"""
Tests where full exercises are run. These are inherently slower.
"""
from pathlib import Path

import pytest

from tests.manual_utils import configuration, execute_config, assert_valid_output


@pytest.mark.slow
@pytest.mark.parametrize("lang", ["python", "java"])
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
@pytest.mark.parametrize("lang", ["haskell", "java", "python"])
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
