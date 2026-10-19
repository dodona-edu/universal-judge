from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config

pytestmark = pytest.mark.python


def test_ruff_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


def test_ruff_error(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "error",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    assert len(annotations) == 1

    assert annotations[0]["type"] == Severity.ERROR
    assert "undefined-name" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 1
    assert annotations[0]["rows"] == 1
    assert annotations[0]["column"] == 11
    assert annotations[0]["columns"] == 18
    # The Dutch text of messages.toml, with the name ruff reported.
    assert annotations[0]["text"].startswith(
        "`undefined_variable` is niet gedefinieerd."
    )
    assert annotations[0]["format"] == "markdown"


def test_ruff_text_in_the_natural_language(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "error",
        {"natural_language": "en", "options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1
    assert annotations[0]["text"].startswith("`undefined_variable` is not defined.")


def test_ruff_warning(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "warning",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1

    assert annotations[0]["type"] == Severity.WARNING
    assert "unused-import" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 0
    assert annotations[0]["rows"] == 1
    # Ruff only marks the imported name, not the whole import statement.
    assert annotations[0]["column"] == 7
    assert annotations[0]["columns"] == 2


def test_ruff_convention(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "convention",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1
    assert annotations[0]["type"] == Severity.INFO
    assert "multiple-statements-on-one-line" in annotations[0]["externalUrl"]


def test_ruff_refactor(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "refactor",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    # Too many arguments (PLR0913), and not also too many positional arguments.
    [annotation] = annotations
    assert "too-many-arguments" in annotation["externalUrl"]

    assert annotation["type"] == Severity.INFO
    assert annotation["row"] == 0
    assert annotation["rows"] == 1
    # Ruff marks the function name, not the whole signature.
    assert annotation["column"] == 4
    assert annotation["columns"] == 17


def test_ruff_syntax_error(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "syntax",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    # One annotation with Python's own message, not those of ruff's parser.
    assert len(annotations) == 1
    assert annotations[0]["text"] == "Syntaxfout: invalid syntax"
    assert annotations[0]["type"] == Severity.ERROR
    # Syntax errors have no rule, and so no documentation page.
    assert annotations[0].get("externalUrl") is None
    assert annotations[0].get("format") is None
    assert annotations[0]["row"] == 0
    assert annotations[0]["rows"] == 1
    assert annotations[0]["column"] == 20


def test_ruff_custom_config(tmp_path: Path, pytestconfig: pytest.Config):
    # This config replaces the judge's rules and only enables F841
    # (unused-variable), so the unused import (F401) in warning.py is not reported.
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "python": {"linter": True, "ruff_config": "ruff_config.toml"}
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 0


def test_ruff_too_deeply_nested_to_compile(tmp_path: Path, pytestconfig: pytest.Config):
    # Python's compiler raises a RecursionError on such a sum, instead of a
    # SyntaxError. What ruff makes of it depends on the platform: on macOS it
    # overflows its stack.
    source = tmp_path / "deep.py"
    source.write_text("x = " + "+".join(["1"] * 10000) + "\n")
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "correct",
        {"source": source, "options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Judging goes on to the compilation, which fails as well.
    statuses = [u["status"]["enum"] for u in updates.find_all("escalate-status")]
    assert "internal error" not in statuses
    assert "compilation error" in statuses


def test_ruff_missing_custom_config_is_reported(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {"python": {"linter": True, "ruff_config": "missing.toml"}}
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 2  # Staff and student warning
    assert len(updates.find_all("annotate-code")) == 0


def test_ruff_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.python.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="invalid json", stderr="", exit=0, timeout=False, memory=False
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 2  # Educator and student warning


def test_ruff_failure_is_reported(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    # Ruff writes nothing to stdout when it fails, which must not be mistaken
    # for a submission without any problems.
    mocker.patch(
        "tested.languages.python.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="",
            stderr="ruff failed\n  Cause: Unknown rule selector `XYZ123`",
            exit=2,
            timeout=False,
            memory=False,
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "python",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 2  # Educator and student warning
    assert len(updates.find_all("annotate-code")) == 0
