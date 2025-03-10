"""
Test full exercises with the parallel option.
"""

from pathlib import Path

import pytest

from tests.language_markers import ALL_LANGUAGES
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", "cpp"])
def test_parallel_isbn(lang: str, tmp_path: Path, pytestconfig: pytest.Config):
    config_ = {"options": {"parallel": True}}
    conf = configuration(
        pytestconfig, "isbn", lang, tmp_path, "full.tson", "solution", options=config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 150
    assert updates.find_status_enum() == ["correct"] * 100


@pytest.mark.parametrize(
    "lang",
    [
        "java",
        "python",
        "kotlin",
        "haskell",
        "runhaskell",
        "cpp"
    ],
)
def test_parallel_isbn_list(lang: str, tmp_path: Path, pytestconfig: pytest.Config):
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


@pytest.mark.parametrize("lang", ["java", "python", "kotlin", "cpp"])
def test_parallel_lotto(lang: str, tmp_path: Path, pytestconfig: pytest.Config):
    config_ = {"options": {"parallel": True}}
    conf = configuration(
        pytestconfig, "lotto", lang, tmp_path, "plan.tson", "correct", options=config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 45
    assert updates.find_status_enum() == ["correct"] * 45


@pytest.mark.parametrize("lang", ALL_LANGUAGES)
def test_parallel_echo(lang: str, tmp_path: Path, pytestconfig: pytest.Config):
    config_ = {"options": {"parallel": True}}
    conf = configuration(
        pytestconfig, "echo", lang, tmp_path, "full.tson", "correct", options=config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 50
    assert updates.find_status_enum() == ["correct"] * 50
