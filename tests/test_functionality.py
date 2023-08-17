"""
Testcases for specific functionality. These tests ensure the judge works.
These tests are a compromise: they don't test the complete output, but this does
make them usable for multiple languages.

Running the tests should happen in with the root directory (the one with src/ and
tests/) as the working directory.
"""
import shutil
import sys
from pathlib import Path

import pytest

from tested.configs import create_bundle
from tested.datatypes import BasicBooleanTypes, BasicNumericTypes, BasicStringTypes
from tested.judge.execution import ExecutionResult
from tested.languages import LANGUAGES
from tested.languages.conventionalize import submission_name
from tested.languages.generation import generate_statement
from tested.serialisation import (
    BooleanType,
    FunctionCall,
    FunctionType,
    NumberType,
    StringType,
)
from tested.testsuite import Suite
from tests.manual_utils import assert_valid_output, configuration, execute_config

COMPILE_LANGUAGES = [
    "python",
    "java",
    "c",
    "kotlin",
    pytest.param("haskell", marks=pytest.mark.haskell),
    "csharp",
]
ALL_SPECIFIC_LANGUAGES = COMPILE_LANGUAGES + [
    "javascript",
    pytest.param("runhaskell", marks=pytest.mark.haskell),
]
ALL_LANGUAGES = ALL_SPECIFIC_LANGUAGES + ["bash"]

quotes = {
    "python": "'",
    "java": '"',
    "c": '"',
    "kotlin": '"',
    "haskell": '"',
    "javascript": '"',
    "runhaskell": '"',
    "bash": "'",
    "csharp": '"',
}


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_global_variable(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "global", language, tmp_path, "one.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_global_variable_yaml(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "global", language, tmp_path, "plan.yaml", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "one.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise_wrong(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one.tson", "wrong")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_simple_programmed_eval(language: str, tmp_path: Path, pytestconfig):
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


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_simple_programmed_eval_wrong(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "one-programmed-wrong.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_file_exercise(language: str, tmp_path: Path, pytestconfig):
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
    language: str, tmp_path: Path, pytestconfig
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


@pytest.mark.parametrize("exercise", ["echo-function-file", "echo-function"])
def test_javascript_async(exercise: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, exercise, "javascript", tmp_path, "one.tson", "correct-async"
    )
    workdir = Path(conf.resources).parent / "workdir"
    if workdir.exists():
        shutil.copytree(workdir, tmp_path, dirs_exist_ok=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_escape_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one-escape.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_display_multiline_exercise(
    language: str, tmp_path: Path, pytestconfig
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
    quote = quotes[language]
    assert expected[0] != quote and expected[-1] != quote
    assert actual[0] != quote and actual[-1] != quote


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_display_no_multiline_exercise(
    language: str, tmp_path: Path, pytestconfig
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
    quote = quotes[language]
    assert expected[0] == quote and expected[-1] == quote
    assert actual[0] == quote and actual[-1] == quote


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_nested_call_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one-nested.yaml", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.haskell
@pytest.mark.parametrize("language", ("haskell", "runhaskell"))
def test_io_function_exercise_haskell_io(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct_io"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_specific_evaluation(language: str, tmp_path: Path, pytestconfig):
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


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_programmed_evaluation(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "programmed-no-haskell.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 5
    assert len(updates.find_all("append-message")) == 5


@pytest.mark.parametrize(
    "lang",
    [
        "python",
        "java",
        "kotlin",
        "csharp",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_language_evaluator_exception_correct(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "division", lang, tmp_path, "plan.json", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize(
    "lang",
    [
        "python",
        "java",
        "kotlin",
        "csharp",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_language_evaluator_generic_exception_correct(
    lang: str, tmp_path: Path, pytestconfig
):
    conf = configuration(
        pytestconfig,
        "division",
        lang,
        tmp_path,
        "plan-generic-exception.json",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize(
    "lang",
    [
        "python",
        "java",
        "kotlin",
        "csharp",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_language_evaluator_exception_wrong(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "division", lang, tmp_path, "plan.json", "wrong")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


@pytest.mark.parametrize(
    "lang",
    [
        "python",
        "java",
        "kotlin",
        "csharp",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_language_evaluator_generic_exception_wrong_error(
    lang: str, tmp_path: Path, pytestconfig
):
    conf = configuration(
        pytestconfig,
        "division",
        lang,
        tmp_path,
        "plan-generic-exception.json",
        "wrong-error",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize(
    "lang",
    [
        "python",
        "java",
        "kotlin",
        "csharp",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_language_evaluator_exception_wrong_error(
    lang: str, tmp_path: Path, pytestconfig
):
    conf = configuration(
        pytestconfig, "division", lang, tmp_path, "plan.json", "wrong-error"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize(
    "lang",
    [
        "python",
        "java",
        "kotlin",
        "csharp",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_language_evaluator_generic_exception_wrong(
    lang: str, tmp_path: Path, pytestconfig
):
    conf = configuration(
        pytestconfig, "division", lang, tmp_path, "plan-generic-exception.json", "wrong"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", "csharp"])
def test_assignment_and_use_in_expression(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "isbn", lang, tmp_path, "one-with-assignment.tson", "solution"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Assert that the empty context testcase is not shown, while the assignment
    # and expression testcase are shown.
    assert len(updates.find_all("start-testcase")) == 2
    # Assert the only one test was executed.
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("start-test")) == 1


@pytest.mark.parametrize(
    "lang",
    [
        "python",
        "java",
        "kotlin",
        "csharp",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_assignment_and_use_in_expression_list(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "isbn-list",
        lang,
        tmp_path,
        "one-with-assignment.tson",
        "solution",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Assert that the empty context testcase is not shown, while the assignment
    # and expression testcase are shown.
    assert len(updates.find_all("start-testcase")) == 2
    # Assert the only one test was executed.
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("start-test")) == 1


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", "csharp"])
def test_crashing_assignment_with_before(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "isbn",
        lang,
        tmp_path,
        f"one-with-crashing-assignment-{lang}.tson",
        "solution",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Only the assignment was started.
    assert len(updates.find_all("start-testcase")) == 2
    print(updates.find_status_enum())
    assert updates.find_status_enum() == ["runtime error", "runtime error", "wrong"]
    # Assert the exception is included.
    assert updates.find_next("start-test")["channel"] == "exception"


@pytest.mark.parametrize(
    "lang",
    [
        "c",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_heterogeneous_arguments_are_detected(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "full.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 0
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.parametrize("lang", ["python", "javascript"])
def test_missing_key_types_detected(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "objects", lang, tmp_path, "missing_key_types.yaml", "solution"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 0
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", "javascript", "csharp"])
def test_programmed_evaluator_lotto(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "lotto", lang, tmp_path, "one-programmed-python.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", "javascript", "csharp"])
def test_programmed_evaluator_wrong(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "lotto", lang, tmp_path, "one-programmed-python.tson", "wrong"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_context_compilation(language: str, tmp_path: Path, pytestconfig, mocker):
    config_ = {"options": {"mode": "context"}}
    # Mock the compilation callback to ensure we call it for every context.
    lang_class = LANGUAGES[language]
    spy = mocker.spy(lang_class, "compilation")
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "two.tson", "correct", config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    assert updates.find_status_enum() == ["correct"] * 2
    assert spy.call_count == 2


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation(language: str, tmp_path: Path, pytestconfig, mocker):
    config_ = {"options": {"mode": "batch"}}
    lang_class = LANGUAGES[language]
    spy = mocker.spy(lang_class, "compilation")
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "two.tson", "correct", config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    assert updates.find_status_enum() == ["correct"] * 2
    assert spy.call_count == 1


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation_fallback(
    language: str, tmp_path: Path, pytestconfig, mocker
):
    lang_class = LANGUAGES[language]
    spy = mocker.spy(lang_class, "compilation")
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "two.tson", "comp-error"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    assert updates.find_status_enum() == ["compilation error"] * 2
    assert spy.call_count == 3


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation_no_fallback(
    language: str, tmp_path: Path, pytestconfig, mocker
):
    config_ = {"options": {"allow_fallback": False}}
    lang_class = LANGUAGES[language]
    spy = mocker.spy(lang_class, "compilation")
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "two.tson", "comp-error", config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-tab")) == 1
    assert updates.find_status_enum() == ["compilation error"]
    assert spy.call_count == 1


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation_no_fallback_runtime(
    language: str, tmp_path: Path, pytestconfig
):
    config_ = {"options": {"allow_fallback": False}}
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "two.tson", "run-error", config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    # One wrong status for every stderr + stdout
    assert len(updates.find_status_enum()) >= 4
    # There could be more wrongs: some languages might modify the exit code
    assert all(s in ("runtime error", "wrong") for s in updates.find_status_enum())


@pytest.mark.parametrize(
    "lang", ["python", "java", "c", "javascript", "kotlin", "bash", "csharp"]
)
def test_program_params(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "sum", lang, tmp_path, "short.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct", "correct", "correct", "correct"]
    assert len(updates.find_all("start-testcase")) == 3
    assert len(updates.find_all("start-test")) == 4


@pytest.mark.parametrize(
    "language", ["python", "java", "kotlin", "javascript", "csharp"]
)
def test_objects(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "objects", language, tmp_path, "plan.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 2
    assert len(updates.find_all("start-testcase")) == 3


@pytest.mark.parametrize(
    "language", ["python", "java", "kotlin", "javascript", "csharp"]
)
def test_objects_chained(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "objects", language, tmp_path, "chained.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 3
    assert len(updates.find_all("start-testcase")) == 3


@pytest.mark.parametrize(
    "language", ["python", "java", "kotlin", "javascript", "csharp"]
)
def test_counter(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "counter", language, tmp_path, "plan.yaml", "solution"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 3
    assert len(updates.find_all("start-testcase")) == 7


@pytest.mark.parametrize(
    "language", ["python", "java", "kotlin", "javascript", "csharp"]
)
def test_counter_chained(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "counter", language, tmp_path, "chained.yaml", "solution"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 3
    assert len(updates.find_all("start-testcase")) == 4


@pytest.mark.parametrize(
    "language", ["python", "java", "kotlin", "javascript", "csharp"]
)
def test_objects_yaml(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "objects", language, tmp_path, "plan.yaml", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 2
    assert len(updates.find_all("start-testcase")) == 3


@pytest.mark.parametrize(
    "language",
    [
        "c",
        pytest.param("haskell", marks=pytest.mark.haskell),
        pytest.param("runhaskell", marks=pytest.mark.haskell),
    ],
)
def test_objects_error(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "objects", language, tmp_path, "plan.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]


def test_too_much_output(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "two.tson",
        "output_limit",
        {"output_limit": 1000},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # 4 times: two times for the tests, one escalate and one judgement.
    assert updates.find_status_enum() == ["output limit exceeded"] * 4
    assert len(updates.find_all("close-test")) == 2


@pytest.mark.parametrize(
    "language,result",
    [
        ("python", ["correct"]),
        ("csharp", ["correct"]),
        ("java", ["internal error"]),
        ("c", ["internal error"]),
        ("javascript", ["internal error"]),
        ("haskell", ["internal error"]),
        ("runhaskell", ["internal error"]),
    ],
)
def test_named_parameters(language: str, result: list, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one-named.tson", "correct"
    )
    all_results = execute_config(conf)
    updates = assert_valid_output(all_results, pytestconfig)
    assert updates.find_status_enum() == result


def test_javascript_exception_correct(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 0


def test_javascript_exception_correct_temp(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "correct-temp",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 0


def test_javascript_exception_wrong(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "wrong",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


def test_javascript_exception_wrong_null(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "wrong-null",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 0


def test_javascript_exception_missing_message(tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "wrong-message",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


def test_timeouts_propagate_to_contexts():
    execution_result = ExecutionResult(
        stdout="--PaqJwrEn0-- SEP--pBoq4YdEP-- SEP",
        stderr="--PaqJwrEn0-- SEP--pBoq4YdEP-- SEP",
        exit=0,
        timeout=True,
        memory=False,
        context_separator="--PaqJwrEn0-- SEP",
        testcase_separator="--pBoq4YdEP-- SEP",
        results="--PaqJwrEn0-- SEP--pBoq4YdEP-- SEP",
        exceptions="--PaqJwrEn0-- SEP--pBoq4YdEP-- SEP",
    )
    context_results = execution_result.to_context_results()
    assert len(context_results) == 1
    context_result = context_results[0]
    assert context_result.timeout
    assert not context_result.memory
    assert context_result.exit == 0
    assert context_result.stdout == execution_result.testcase_separator
    assert context_result.stderr == execution_result.testcase_separator
    assert context_result.results == execution_result.testcase_separator
    assert context_result.exceptions == execution_result.testcase_separator


def test_function_arguments_without_brackets(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", "haskell", tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)

    statement = FunctionCall(
        type=FunctionType.FUNCTION,
        name="test",
        namespace=None,
        arguments=[
            NumberType(type=BasicNumericTypes.REAL, data=5.5),
            StringType(type=BasicStringTypes.TEXT, data="hallo"),
            BooleanType(type=BasicBooleanTypes.BOOLEAN, data=True),
        ],
    )

    result = generate_statement(bundle, statement)
    assert (
        result
        == f'{submission_name(bundle.lang_config)}.test 5.5 :: Double "hallo" True'
    )


@pytest.mark.parametrize(
    "language_and_expected",
    [
        ("csharp", '(Coords) {"X":5.5,"Y":7.5}'),
        ("java", "Coord[x=5, y=7]"),
        ("javascript", '(Coord) {"x":5,"y":7}'),
        ("kotlin", "Coord(x=5, y=6)"),
        ("python", "(<class 'submission.Coord'>) Coord(x=5, y=6)"),
    ],
)
def test_unknown_return_type(tmp_path: Path, pytestconfig, language_and_expected):
    language, expected = language_and_expected
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "one.tson",
        "unknown-return-type",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    received_data = updates.find_next("close-test")["generated"]
    assert received_data == expected


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_expected_no_return_but_got_some(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "expected_no_return_but_got_some.yaml",
        "correct",
    )

    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_expected_no_return_and_got_none(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "expected_no_return_and_got_none.yaml",
        "correct",
    )

    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_expected_return_but_got_none(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "expected_return_but_got_none.yaml",
        "correct",
    )

    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_expected_return_and_got_some(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "expected_return_and_got_some.yaml",
        "correct",
    )

    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_ignored_return_and_got_some(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "ignored_return_but_got_some.json",
        "correct",
    )

    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == []  # Empty means correct
