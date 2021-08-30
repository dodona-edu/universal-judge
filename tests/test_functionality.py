"""
Testcases for specific functionality. These tests ensure the judge works.
These tests are a compromise: they don't test the complete output, but this does
make them usable for multiple languages.

The configure which languages are tested, modify the `conftest.py` file.

Running the tests should happen in with the root directory (the one with src/ and
tests/) as the working directory.
"""
from pathlib import Path

import pytest
import shutil

from tested.languages import LANGUAGES
from tests.manual_utils import assert_valid_output, configuration, execute_config, mark_haskell

COMPILE_LANGUAGES = ["python", "java", "c", "kotlin", pytest.param("haskell", marks=mark_haskell)]
ALL_SPECIFIC_LANGUAGES = COMPILE_LANGUAGES + ["javascript", pytest.param("runhaskell", marks=mark_haskell)]
ALL_LANGUAGES = ALL_SPECIFIC_LANGUAGES + ["bash"]

quotes = {
    "python":     "'",
    "java":       '"',
    "c":          '"',
    "kotlin":     '"',
    "haskell":    '"',
    "javascript": '"',
    "runhaskell": '"',
    "bash":       '"'
}


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_global_variable(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "global", language, tmp_path, "one.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_global_variable_yaml(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "global", language, tmp_path, "plan.yaml", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one.tson", "correct")
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
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one-programmed-correct.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise_io_optimized(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "two.tson", "correct",
                         options={
                             "options": {
                                 "optimized_io": True
                             }
                         })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 2


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise_io_optimized_programmed(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one-programmed-correct.tson", "correct",
                         options={
                             "options": {
                                 "optimized_io": True
                             }
                         })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise_io_optimized_comp(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one.tson", "comp-error",
                         options={
                             "options": {
                                 "optimized_io": True
                             }
                         })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["compilation error"] * 3


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise_io_optimized_run(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one.tson", "run-error",
                         options={
                             "options": {
                                 "optimized_io": True
                             }
                         })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"] * (2 if language == 'c' else 3)


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise_io_optimized_wrong(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one.tson", "wrong",
                         options={
                             "options": {
                                 "optimized_io": True
                             }
                         })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_simple_programmed_eval_wrong(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one-programmed-wrong.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_file_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function-file", language, tmp_path, "one.tson", "correct")
    shutil.copytree(Path(conf.resources).parent / "workdir", tmp_path, dirs_exist_ok=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_additional_source_files(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function-additional-source-files", language, tmp_path, "one.tson",
                         "correct")
    shutil.copytree(Path(conf.resources).parent / "workdir", tmp_path, dirs_exist_ok=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("exercise", ["echo-function-file", "echo-function"])
def test_javascript_async(exercise: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, exercise, "javascript", tmp_path, "one.tson", "correct-async")
    workdir = Path(conf.resources).parent / "workdir"
    if workdir.exists():
        shutil.copytree(workdir, tmp_path, dirs_exist_ok=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_escape_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one-escape.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_display_multiline_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one-display-multiline.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    start_test = updates.find_all('start-test')
    close_test = updates.find_all('close-test')
    assert 1 == len(start_test)
    assert 1 == len(close_test)
    assert "return (String)" == start_test[0].get("channel", '')
    expected, actual = start_test[0].get("expected", ''), close_test[0].get("generated", '')
    quote = quotes[language]
    assert expected[0] != quote and expected[-1] != quote
    assert actual[0] != quote and actual[-1] != quote


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_display_no_multiline_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one-display-no-multiline.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    start_test = updates.find_all('start-test')
    close_test = updates.find_all('close-test')
    assert 1 == len(start_test)
    assert 1 == len(close_test)
    assert "return" == start_test[0].get("channel", '')
    expected, actual = start_test[0].get("expected", ''), close_test[0].get("generated", '')
    quote = quotes[language]
    assert expected[0] == quote and expected[-1] == quote
    assert actual[0] == quote and actual[-1] == quote


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_nested_call_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one-nested.yaml", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@mark_haskell
@pytest.mark.parametrize("language", ("haskell", "runhaskell"))
def test_io_function_exercise_haskell_io(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct_io")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_specific_evaluation(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "two-specific.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong", "correct"]
    assert len(updates.find_all("append-message")) == 2


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_programmed_evaluation(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "programmed-no-haskell.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 4
    assert len(updates.find_all("append-message")) == 4


@pytest.mark.parametrize("lang", ["python", "java", "javascript", "kotlin", pytest.param("haskell", marks=mark_haskell),
                                  pytest.param("runhaskell", marks=mark_haskell)])
def test_language_evaluator_exception(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "division", lang, tmp_path, "plan.json", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", pytest.param("haskell", marks=mark_haskell),
                                  pytest.param("runhaskell", marks=mark_haskell)])
def test_language_evaluator_exception(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "division", lang, tmp_path, "plan.json", "wrong")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


@pytest.mark.parametrize("lang", ["python", "java", "kotlin"])
def test_assignment_and_use_in_expression(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "one-with-assignment.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Assert that the empty context testcase is not shown, while the assignment
    # and expression testcase are shown.
    assert len(updates.find_all("start-testcase")) == 2
    # Assert the only one test was executed.
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("start-test")) == 1


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", pytest.param("haskell", marks=mark_haskell),
                                  pytest.param("runhaskell", marks=mark_haskell)])
def test_assignment_and_use_in_expression_list(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "isbn-list", lang, tmp_path, "one-with-assignment.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Assert that the empty context testcase is not shown, while the assignment
    # and expression testcase are shown.
    assert len(updates.find_all("start-testcase")) == 2
    # Assert the only one test was executed.
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("start-test")) == 1


@pytest.mark.parametrize("lang", ["python", "java", "kotlin"])
def test_crashing_assignment_with_before(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, f"one-with-crashing-assignment-{lang}.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Only the assignment was started.
    assert len(updates.find_all("start-testcase")) == 2
    print(updates.find_status_enum())
    assert updates.find_status_enum() == ["runtime error", "runtime error", "wrong"]
    # Assert the exception is included.
    assert updates.find_next("start-test")["channel"] == "exception"


@pytest.mark.parametrize("lang", ["c", pytest.param("haskell", marks=mark_haskell),
                                  pytest.param("runhaskell", marks=mark_haskell)])
def test_heterogeneous_arguments_are_detected(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "full.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 0
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.parametrize("lang", ["python", "javascript"])
def test_missing_key_types_detected(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "objects", lang, tmp_path, "missing_key_types.yaml", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 0
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", "javascript"])
def test_programmed_evaluator_lotto(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "lotto", lang, tmp_path, "one-programmed-python.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", "javascript"])
def test_programmed_evaluator_wrong(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "lotto", lang, tmp_path, "one-programmed-python.tson", "wrong")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_context_compilation(language: str, tmp_path: Path, pytestconfig, mocker):
    config_ = {
        "options": {
            "mode": "context"
        }
    }
    # Mock the compilation callback to ensure we call it for every context.
    lang_class = LANGUAGES[language]
    class_instance = lang_class()
    mocker.patch.object(lang_class, 'compilation', wraps=class_instance.compilation)
    conf = configuration(pytestconfig, "echo", language, tmp_path, "two.tson", "correct", config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    assert updates.find_status_enum() == ["correct"] * 2
    assert class_instance.compilation.call_count == 2


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation(language: str, tmp_path: Path, pytestconfig, mocker):
    config_ = {
        "options": {
            "mode": "batch"
        }
    }
    lang_class = LANGUAGES[language]
    class_instance = lang_class()
    mocker.patch.object(lang_class, 'compilation', wraps=class_instance.compilation)
    conf = configuration(pytestconfig, "echo", language, tmp_path, "two.tson", "correct", config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    assert updates.find_status_enum() == ["correct"] * 2
    assert class_instance.compilation.call_count == 1


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation_fallback(language: str, tmp_path: Path, pytestconfig, mocker):
    lang_class = LANGUAGES[language]
    class_instance = lang_class()
    mocker.patch.object(lang_class, 'compilation', wraps=class_instance.compilation)
    conf = configuration(pytestconfig, "echo", language, tmp_path, "two.tson", "comp-error")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    assert updates.find_status_enum() == ["compilation error"] * 2
    assert class_instance.compilation.call_count == 3


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation_no_fallback(language: str, tmp_path: Path, pytestconfig, mocker):
    config_ = {
        "options": {
            "allow_fallback": False
        }
    }
    lang_class = LANGUAGES[language]
    class_instance = lang_class()
    mocker.patch.object(lang_class, 'compilation', wraps=class_instance.compilation)
    conf = configuration(pytestconfig, "echo", language, tmp_path, "two.tson", "comp-error", config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    assert updates.find_status_enum() == ["compilation error"] * 4
    assert class_instance.compilation.call_count == 1


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation_no_fallback_runtime(language: str, tmp_path: Path, pytestconfig):
    config_ = {
        "options": {
            "allow_fallback": False
        }
    }
    conf = configuration(pytestconfig, "echo", language, tmp_path, "two.tson", "run-error", config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    # One wrong status for every stderr + stdout
    assert len(updates.find_status_enum()) >= 4
    # There could be more wrongs: some languages might modify the exit code
    assert all(s in ("runtime error", "wrong") for s in updates.find_status_enum())


@pytest.mark.parametrize("lang", ["python", "java", "c", "javascript", "kotlin", "bash"])
def test_program_params(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "sum", lang, tmp_path, "short.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ['correct', 'correct', 'correct', 'correct']
    assert len(updates.find_all("start-testcase")) == 3
    assert len(updates.find_all("start-test")) == 4


@pytest.mark.parametrize("lang", ["python", "java", "c", "javascript", "kotlin", "bash"])
def test_program_params_optimized_io(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "sum", lang, tmp_path, "short.tson", "correct", options={
        "optimized_io": True
    })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ['correct', 'correct', 'correct', 'correct']
    assert len(updates.find_all("start-testcase")) == 3
    assert len(updates.find_all("start-test")) == 4


@pytest.mark.parametrize("language", ["python", "java", "kotlin", "javascript"])
def test_objects(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "objects", language, tmp_path, "plan.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 2
    assert len(updates.find_all("start-testcase")) == 3


@pytest.mark.parametrize("language", ["python", "java", "kotlin", "javascript"])
def test_objects_chained(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "objects", language, tmp_path, "chained.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 3
    assert len(updates.find_all("start-testcase")) == 3


@pytest.mark.parametrize("language", ["python", "java", "kotlin", "javascript"])
def test_counter(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "counter", language, tmp_path, "plan.yaml", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 3
    assert len(updates.find_all("start-testcase")) == 7


@pytest.mark.parametrize("language", ["python", "java", "kotlin", "javascript"])
def test_counter_chained(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "counter", language, tmp_path, "chained.yaml", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 3
    assert len(updates.find_all("start-testcase")) == 4


@pytest.mark.parametrize("language", ["python", "java", "kotlin", "javascript"])
def test_objects_yaml(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "objects", language, tmp_path, "plan.yaml", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 2
    assert len(updates.find_all("start-testcase")) == 3


@pytest.mark.parametrize("language", ["c", pytest.param("haskell", marks=mark_haskell),
                                      pytest.param("runhaskell", marks=mark_haskell)])
def test_objects_error(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "objects", language, tmp_path, "plan.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]


def test_too_much_output(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", "python", tmp_path, "two.tson", "output_limit", {
        "output_limit": 1000
    })
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # 4 times: two times for the tests, one escalate and one judgement.
    assert updates.find_status_enum() == ["output limit exceeded"] * 4
    assert len(updates.find_all("close-test")) == 2


def test_named_parameters_supported(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path, "one-named.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ["java", "c", "javascript", pytest.param("haskell", marks=mark_haskell),
                                      pytest.param("runhaskell", marks=mark_haskell)])
def test_named_parameters_not_supported(language, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one-named.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_hide_expected_correct(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one-hide-expected.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(list(filter(lambda x: bool(x["expected"]), updates.find_all("start-test")))) == 1


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_hide_expected_wrong(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one-hide-expected.tson", "wrong")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(list(filter(lambda x: not bool(x["expected"]), updates.find_all("start-test")))) == 1


def test_javascript_exception_correct(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "js-exceptions", "javascript", tmp_path, "plan.yaml", "correct",
                         backward_compatibility_plan=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 0


def test_javascript_exception_wrong(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "js-exceptions", "javascript", tmp_path, "plan.yaml", "wrong",
                         backward_compatibility_plan=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


def test_javascript_exception_missing_message(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "js-exceptions", "javascript", tmp_path, "plan.yaml", "wrong-message",
                         backward_compatibility_plan=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1
