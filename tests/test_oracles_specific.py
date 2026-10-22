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


def test_specific_oracle_shows_a_list_answer_as_wrong(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "two-specific.tson",
        "wrong-list",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # The evaluator passes the student's list as its readable actual value.
    assert updates.find_status_enum() == ["wrong", "wrong"]
    generated = [t["generated"] for t in updates.find_all("close-test")]
    assert generated == ["['input-1']", "['correct']"]


@pytest.mark.parametrize(
    ("language", "solution", "shown"),
    [
        ("python", "wrong-int", "5"),
        ("python", "wrong-float", "2.5"),
        ("python", "wrong-bool", "True"),
        ("javascript", "wrong-int", "5"),
        ("typescript", "wrong-int", "5"),
        ("javascript", "wrong-list", "[5]"),
        ("typescript", "wrong-list", "[5]"),
        ("javascript", "wrong-bare-object", "{'a': 1}"),
        ("typescript", "wrong-bare-object", "{'a': 1}"),
    ],
)
def test_specific_oracle_shows_a_scalar_answer_as_wrong(
    language: str,
    solution: str,
    shown: str,
    tmp_path: Path,
    pytestconfig: pytest.Config,
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "specific-raw-readable.yaml",
        solution,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # The evaluators pass the student's value as their readable actual value.
    assert updates.find_status_enum() == ["wrong"]
    generated = [t["generated"] for t in updates.find_all("close-test")]
    assert generated == [shown]


def test_specific_oracle_messages_keep_their_permission(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "specific-permissions.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = [m["message"] for m in updates.find_all("append-message")]
    # Dodona's former "staff" is still accepted, as "educator": a message that
    # isn't understood would become text that students see.
    assert messages == [
        {
            "description": "for educators (staff)",
            "format": "text",
            "permission": "educator",
        },
        {"description": "for educators", "format": "text", "permission": "educator"},
        {"description": "for everyone", "format": "text"},
    ]


def test_specific_oracle_message_with_unknown_permission_is_not_shown_to_students(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "specific-unknown-permission.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # The oracle's result can't be read, so only educators see it.
    assert updates.find_status_enum() == ["internal error"]
    messages = [m["message"] for m in updates.find_all("append-message")]
    shown = [m for m in messages if "secret for teachers" in str(m)]
    assert shown
    assert all(isinstance(m, dict) and m.get("permission") == "educator" for m in shown)


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


@pytest.mark.parametrize("language", ALL_SPECIFIC_LANGUAGES)
def test_specific_oracle_return_specific_argument_dsl(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "one-specific-argument.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 1
