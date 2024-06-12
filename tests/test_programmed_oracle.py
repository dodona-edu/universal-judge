"""
Tests for programmed oracles (also known as custom check functions).
"""

from pathlib import Path

import pytest
from language_markers import ALL_LANGUAGES

from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_custom_check_function_stdout(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo",
        language,
        tmp_path,
        "one-programmed-correct.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_custom_check_function_stdout_wrong_result(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "echo", "python", tmp_path, "one-programmed-wrong.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_custom_check_function_return(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "programmed.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message"))


def test_custom_check_function_runtime_crash(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "programmed_crash.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]
    assert len(updates.find_all("append-message")) == 4


def test_custom_check_function_syntax_error(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "programmed_syntax_error.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]
    assert len(updates.find_all("append-message")) == 4


def test_missing_custom_check_function(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "programmed_missing.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]
    assert len(updates.find_all("append-message")) == 4
