"""
Testcases for specific functionality. These tests ensure the judge works.
These tests are a compromise: they don't test the complete output, but this does
make them usable for multiple languages.

Running the tests should happen in with the root directory (the one with src/ and
tests/) as the working directory.
"""

import sys
from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.configs import create_bundle
from tested.features import Construct
from tested.judge.execution import ExecutionResult
from tested.languages import get_language, LANGUAGES
from tested.languages.generation import get_readable_input
from tested.testsuite import Context, MainInput, Suite, Tab, Testcase, TextData
from tests.language_markers import (
    ALL_LANGUAGES,
    ALL_SPECIFIC_LANGUAGES,
    EXCEPTION_LANGUAGES,
)
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_global_variable(language: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "global", language, tmp_path, "one.tson", "correct"
    )
    if Construct.GLOBAL_VARIABLES not in get_language(None, conf.programming_language).supported_constructs():
        pytest.skip("Language doesn't support global variables")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_global_variable_yaml(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "global", language, tmp_path, "plan.yaml", "correct"
    )
    if Construct.GLOBAL_VARIABLES not in get_language(None, conf.programming_language).supported_constructs():
        pytest.skip("Language doesn't support global variables")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", EXCEPTION_LANGUAGES)
def test_generic_exception_wrong(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "division", lang, tmp_path, "plan-generic-exception.json", "wrong"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("lang", EXCEPTION_LANGUAGES)
def test_generic_exception_correct(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
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


@pytest.mark.parametrize("lang", EXCEPTION_LANGUAGES)
def test_generic_exception_wrong_error(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
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


@pytest.mark.parametrize("lang", ["python", "java", "kotlin", "csharp"])
def test_assignment_and_use_in_expression(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
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
def test_assignment_and_use_in_expression_list(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
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
def test_crashing_assignment_with_before(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
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
def test_heterogeneous_arguments_are_detected(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "full.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 0
    assert updates.find_status_enum() == ["internal error"]


def test_missing_key_types_detected(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "objects", "python", tmp_path, "missing_key_types.yaml", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 0
    assert updates.find_status_enum() == ["internal error"]


def test_missing_key_types_detected_js_object(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "objects",
        "javascript",
        tmp_path,
        "missing_key_types_js_object.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 0
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.parametrize(
    "suite", ["missing_key_types_js_dictionary", "missing_key_types"]
)
def test_missing_key_types_detected_js_dictionary(
    suite: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "objects", "javascript", tmp_path, f"{suite}.yaml", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", ["java"])
def test_advanced_types_are_allowed(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "objects",
        lang,
        tmp_path,
        "advanced_values_in_set.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation(
    language: str, tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
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
    language: str, tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    config_ = {"options": {"allow_fallback": True}}
    lang_class = LANGUAGES[language]
    spy = mocker.spy(lang_class, "compilation")
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "two.tson", "comp-error", config_
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 2
    assert updates.find_status_enum() == ["compilation error"] * 2
    assert spy.call_count == 3


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation_no_fallback(
    language: str, tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
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
    assert updates.find_status_enum() == ["compilation error"] * 2
    assert spy.call_count == 1


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_batch_compilation_no_fallback_runtime(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
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
def test_program_params(lang: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "sum", lang, tmp_path, "short.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct", "correct", "correct", "correct"]
    assert len(updates.find_all("start-testcase")) == 3
    assert len(updates.find_all("start-test")) == 4


@pytest.mark.parametrize(
    "language", ["python", "java", "kotlin", "javascript", "csharp"]
)
def test_objects(language: str, tmp_path: Path, pytestconfig: pytest.Config):
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
def test_objects_chained(language: str, tmp_path: Path, pytestconfig: pytest.Config):
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
def test_property_assignment(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "objects",
        language,
        tmp_path,
        "property_assignment.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 1
    assert len(updates.find_all("start-testcase")) == 3


@pytest.mark.parametrize(
    "language", ["python", "java", "kotlin", "javascript", "csharp"]
)
def test_counter(language: str, tmp_path: Path, pytestconfig: pytest.Config):
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
def test_counter_chained(language: str, tmp_path: Path, pytestconfig: pytest.Config):
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
def test_objects_yaml(language: str, tmp_path: Path, pytestconfig: pytest.Config):
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
def test_objects_error(language: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "objects", language, tmp_path, "plan.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.parametrize(
    "language,result",
    [
        ("python", ["correct"]),
        ("csharp", ["correct"]),
        ("java", ["internal error"]),
        ("c", ["internal error"]),
        ("javascript", ["correct"]),
        ("haskell", ["internal error"]),
        ("runhaskell", ["internal error"]),
    ],
)
def test_named_parameters(
    language: str, result: list, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo-function", language, tmp_path, "one-named.tson", "correct"
    )
    all_results = execute_config(conf)
    updates = assert_valid_output(all_results, pytestconfig)
    assert updates.find_status_enum() == result


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
def test_unknown_return_type(
    tmp_path: Path, pytestconfig: pytest.Config, language_and_expected: tuple[str, str]
):
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
def test_expected_no_return_but_got_some(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
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
def test_expected_no_return_and_got_none(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
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
    assert updates.find_status_enum() == []


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_expected_return_but_got_none(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
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
def test_expected_return_and_got_some(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
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
def test_ignored_return_and_got_some(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
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


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_language_literals_work(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "one-language-literals.yaml",
        "correct",
    )

    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


# Check that the test suite is valid with a correct submission.
# This test suite is used for the test below "test_output_in_script_is_caught".
@pytest.mark.parametrize("language", ["python", "javascript", "bash"])
def test_two_suite_is_valid(language: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "two.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 2


@pytest.mark.parametrize("language", ["python", "javascript", "bash"])
def test_output_in_script_is_caught(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "two.yaml",
        "top-level-output",
    )
    result = execute_config(conf)
    print(result)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong", "correct", "correct"]


def test_main_call_quotes(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "bash",
        tmp_path,
        "two.yaml",
        "top-level-output",
    )
    the_input = Testcase(
        input=MainInput(arguments=["hello", "it's", "$yes", "--hello=no", "-hello"])
    )
    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="hallo")])
    bundle = create_bundle(conf, sys.stdout, suite)
    actual, _ = get_readable_input(bundle, the_input)

    assert (
        actual.description == "$ submission hello 'it'\"'\"'s' '$yes' --hello=no -hello"
    )


def test_stdin_and_arguments_use_heredoc(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "bash",
        tmp_path,
        "two.yaml",
        "top-level-output",
    )
    the_input = Testcase(
        input=MainInput(
            arguments=["hello"], stdin=TextData(data="One line\nSecond line\n")
        )
    )
    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="hallo")])
    bundle = create_bundle(conf, sys.stdout, suite)
    actual, _ = get_readable_input(bundle, the_input)

    assert (
        actual.description
        == "$ submission hello << 'STDIN'\nOne line\nSecond line\nSTDIN"
    )


def test_stdin_token_is_unique(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "bash",
        tmp_path,
        "two.yaml",
        "top-level-output",
    )
    the_input = Testcase(
        input=MainInput(arguments=["hello"], stdin=TextData(data="One line\nSTDIN\n"))
    )
    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="hallo")])
    bundle = create_bundle(conf, sys.stdout, suite)
    actual, _ = get_readable_input(bundle, the_input)

    assert (
        actual.description == "$ submission hello << 'STDINN'\nOne line\nSTDIN\nSTDINN"
    )
