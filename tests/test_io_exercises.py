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
def test_io_exercise_stdin(language: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "plan.yaml", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 3


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise_input_dynamic_file(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "plan-dynamic.yaml", "correct-files"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 6


def test_io_exercise_input_dynamic_file_nested_path(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # Regression test: ContentPath source with a nested destination path (e.g.
    # "subdir/input.txt") requires creating the parent directory before copying.
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan-dynamic-nested-path.yaml",
        "correct-files-nested",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_io_exercise_stdin_multiline_args(tmp_path: Path, pytestconfig: pytest.Config):
    # Exercises the heredoc display path (stdin.count('\n') > 1) in get_readable_input().
    # Python-only: other correct.* solutions read all of stdin and would produce different output.
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan-stdin-multiline-args.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_io_exercise_lax_workdir_has_workdir_files(
    tmp_path: Path, pytestconfig: pytest.Config
):
    (tmp_path / "workdir-extra.txt").write_text("workdir content\n")
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan-lax-workdir.yaml",
        "correct-workdir",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_io_exercise_strict_workdir_isolates_workdir_files(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # workdir-extra.txt is present in the working dir, but strict mode prevents it
    # from reaching the execution directory since it is not listed in input_files.
    (tmp_path / "workdir-extra.txt").write_text("workdir content\n")
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan-strict-isolation.yaml",
        "correct-workdir",
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
def test_io_function_file_input_exercise(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function-file-input",
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
def test_io_function_file_output_exercise(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function-file-output",
        language,
        tmp_path,
        "one.yaml",
        "correct",
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


def test_path_traversal_rejected(tmp_path: Path, pytestconfig: pytest.Config):
    plan_content = """\
- tab: "Traversal"
  testcases:
    - stdin: "hello"
      input_files:
        - path: "../../escape.txt"
          content: "data"
      stdout: "hello"
"""
    plan_file = tmp_path / "traversal-plan.yaml"
    plan_file.write_text(plan_content)

    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "traversal-plan.yaml",
        "correct",
        options={"resources": tmp_path},
    )

    with pytest.raises(
        AssertionError, match="Cannot write outside the execution directory"
    ):
        execute_config(conf)


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_file_combinations(language: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "time-2-code",
        language,
        tmp_path,
        "plan.yml",
        "solution",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 4
