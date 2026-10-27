"""
Tests specifically for IO exercises.
"""

import shutil
from pathlib import Path

import pytest

from tested.configs import DodonaConfig, _get_language
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


def _shebang_config(
    pytestconfig: pytest.Config, tmp_path: Path, first_line: str
) -> tuple[DodonaConfig, Path]:
    source = tmp_path / "submission.py"
    source.write_text(f"{first_line}\nprint(input())\n")
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan.yaml",
        options={"source": source},
    )
    return conf, source


def test_io_exercise_unknown_shebang_language_is_ignored(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf, source = _shebang_config(pytestconfig, tmp_path, "#!tested foo")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 3
    assert "#!tested" not in source.read_text()


@pytest.mark.parametrize(
    "first_line, expected_offset",
    [("#!tested foo", 1), ("#!tested a b", 1), ("#!tested python", 1), ("# note", 0)],
)
def test_unknown_shebang_language_counts_as_consumed_line(
    first_line: str,
    expected_offset: int,
    tmp_path: Path,
    pytestconfig: pytest.Config,
):
    conf, _ = _shebang_config(pytestconfig, tmp_path, first_line)
    language, offset = _get_language(conf)
    assert language == SupportedLanguage.PYTHON
    assert offset == expected_offset


@pytest.mark.parametrize(
    "files",
    [
        ["common/data.txt"],
        ["execution_0/c.txt"],
        ["execution_logs/a.txt", "data/b.txt"],
    ],
)
def test_io_exercise_workdir_folders_reach_submission(
    files: list[str], tmp_path: Path, pytestconfig: pytest.Config
):
    for file in files:
        (tmp_path / file).parent.mkdir()
        (tmp_path / file).write_text("workdir content\n")
    (tmp_path / "expected-files.txt").write_text("\n".join(files))
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan-workdir-folders.yaml",
        "correct-workdir-folders",
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


def test_csharp_compiles_source_files_in_workdir_subdirectories(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function-additional-source-files",
        "csharp",
        tmp_path,
        "one.tson",
        "correct",
    )
    # dotnet build compiled **/*.cs, so exercises may keep helpers in a subdirectory.
    helpers = tmp_path / "helpers"
    helpers.mkdir()
    shutil.move(tmp_path / "EchoClass.cs", helpers)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_csharp_compile_error_has_no_warnings_from_support_files(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo-function", "csharp", tmp_path, "one.tson", "comp-error"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["compilation error"]
    messages = updates.find_all("append-message")
    assert "CS0103" in messages[0]["message"]["description"]
    assert not any("Values.cs" in m["message"]["description"] for m in messages)


def test_csharp_ignores_dll_files_in_workdir_subdirectories(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function-additional-source-files",
        "csharp",
        tmp_path,
        "one.tson",
        "correct",
    )
    # Only the compiler's output is copied as a dependency: the execution gets
    # the workdir's subdirectories anyway, and copying a file twice crashes.
    data = tmp_path / "data"
    data.mkdir()
    (data / "Library.dll").write_bytes(b"not an assembly")
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
