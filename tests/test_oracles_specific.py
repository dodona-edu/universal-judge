"""
Testcases for language-specific oracles.
"""

from pathlib import Path

import pytest

from tests.language_markers import ALL_SPECIFIC_LANGUAGES, EXCEPTION_LANGUAGES
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_specific_oracle_return(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "two-specific.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong", "correct"]
    assert len(updates.find_all("append-message")) == 2


@pytest.mark.parametrize("lang", EXCEPTION_LANGUAGES)
def test_specific_oracle_exception_correct(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "division", lang, tmp_path, "plan.json", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", EXCEPTION_LANGUAGES)
def test_specific_oracle_exception_wrong(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "division", lang, tmp_path, "plan.json", "wrong")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


@pytest.mark.parametrize("lang", EXCEPTION_LANGUAGES)
def test_specific_oracle_exception_wrong_exception(
    lang: str, tmp_path: Path, pytestconfig
):
    conf = configuration(
        pytestconfig, "division", lang, tmp_path, "plan.json", "wrong-error"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("lang", EXCEPTION_LANGUAGES)
def test_specific_oracle_exception_syntax_error(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "division", lang, tmp_path, "plan-syntax-error.json", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["compilation error"]
    assert len(updates.find_all("append-message")) == 1


@pytest.mark.parametrize("lang", EXCEPTION_LANGUAGES)
def test_specific_oracle_exception_runtime_exception(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "division",
        lang,
        tmp_path,
        "plan-runtime-exception.json",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong", "wrong"]
    assert len(updates.find_all("append-message")) >= 1


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_specific_oracle_return_specific_argument(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "one-specific-argument.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 1
