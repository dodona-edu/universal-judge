"""
Tests where full exercises are run. These are inherently slower.
"""

from pathlib import Path

import pytest

from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.slow
@pytest.mark.parametrize("lang", ["python", "java", "kotlin"])
def test_full_isbn(lang: str, tmp_path: Path, pytestconfig):
    config_ = {"options": {"parallel": True}}
    conf = configuration(
        pytestconfig, "isbn", lang, tmp_path, "full.tson", "solution", options=config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 150
    assert updates.find_status_enum() == ["correct"] * 100


@pytest.mark.slow
@pytest.mark.parametrize(
    "lang",
    [
        "java",
        "python",
        "kotlin",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_full_isbn_list(lang: str, tmp_path: Path, pytestconfig):
    config_ = {"options": {"parallel": True}}
    conf = configuration(
        pytestconfig,
        "isbn-list",
        lang,
        tmp_path,
        "plan.tson",
        "solution",
        options=config_,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 150
    assert updates.find_status_enum() == ["correct"] * 100


@pytest.mark.slow
@pytest.mark.parametrize("lang", ["java", "python", "kotlin"])
def test_full_lotto(lang: str, tmp_path: Path, pytestconfig):
    config_ = {"options": {"parallel": True}}
    conf = configuration(
        pytestconfig, "lotto", lang, tmp_path, "plan.tson", "correct", options=config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 45
    assert updates.find_status_enum() == ["correct"] * 45


@pytest.mark.slow
@pytest.mark.parametrize(
    "lang",
    [
        "java",
        "python",
        "c",
        "javascript",
        "kotlin",
        "bash",
        pytest.param("haskell", marks=pytest.mark.haskell),
    ],
)
def test_full_echo(lang: str, tmp_path: Path, pytestconfig):
    config_ = {"options": {"parallel": True}}
    conf = configuration(
        pytestconfig, "echo", lang, tmp_path, "full.tson", "correct", options=config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 50
    assert updates.find_status_enum() == ["correct"] * 50
