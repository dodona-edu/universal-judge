"""
Tests specifically for IO exercises.
"""

import shutil
from pathlib import Path

import pytest

from tested.languages.language import STRING_QUOTES
from tested.testsuite import SupportedLanguage
from tests.language_markers import ALL_LANGUAGES
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise(language: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "one.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise_wrong(language: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one.tson", "wrong")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_exercise(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_file_exercise(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo-function-file", language, tmp_path, "one.tson", "correct"
    )
    shutil.copytree(
        Path(conf.resources).parent / "workdir", tmp_path, dirs_exist_ok=True
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_additional_source_files(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function-additional-source-files",
        language,
        tmp_path,
        "one.tson",
        "correct",
    )
    shutil.copytree(
        Path(conf.resources).parent / "workdir", tmp_path, dirs_exist_ok=True
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_escape_exercise(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one-escape.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_display_multiline_exercise(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "one-display-multiline.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    start_test = updates.find_all("start-test")
    close_test = updates.find_all("close-test")
    assert 1 == len(start_test)
    assert 1 == len(close_test)
    assert "return" == start_test[0].get("channel", "")
    expected, actual = start_test[0].get("expected", ""), close_test[0].get(
        "generated", ""
    )
    quote = STRING_QUOTES[SupportedLanguage(language)]
    assert expected[0] != quote and expected[-1] != quote
    assert actual[0] != quote and actual[-1] != quote


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_display_no_multiline_exercise(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "one-display-no-multiline.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    start_test = updates.find_all("start-test")
    close_test = updates.find_all("close-test")
    assert 1 == len(start_test)
    assert 1 == len(close_test)
    assert "return" == start_test[0].get("channel", "")
    expected, actual = start_test[0].get("expected", ""), close_test[0].get(
        "generated", ""
    )
    quote = STRING_QUOTES[SupportedLanguage(language)]
    assert expected[0] == quote and expected[-1] == quote
    assert actual[0] == quote and actual[-1] == quote


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_nested_call_exercise(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one-nested.yaml", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ("haskell", "runhaskell"))
def test_io_function_exercise_haskell_io(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct_io"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
