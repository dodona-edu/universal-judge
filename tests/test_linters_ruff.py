from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config


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
    assert len(annotations) == 1

    assert annotations[0]["type"] == Severity.INFO
    assert "too-many-arguments" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 0
    assert annotations[0]["rows"] == 1
    # Ruff marks the function name, not the whole signature.
    assert annotations[0]["column"] == 4
    assert annotations[0]["columns"] == 17


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
    assert len(annotations) > 0

    # Syntax errors have no rule, and so no documentation page.
    assert all(a["type"] == Severity.ERROR for a in annotations)
    assert all(a.get("externalUrl") is None for a in annotations)
    # The annotation must not run past the line the error is on.
    assert all(a["row"] == 0 for a in annotations)
    assert all(a["rows"] == 1 for a in annotations)


def test_ruff_custom_config(tmp_path: Path, pytestconfig: pytest.Config):
    # This config only enables F841 (unused-variable).
    # Our warning.py has an unused import (F401), which should be disabled.
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


def test_ruff_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.python.ruff_linter.run_command",
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
    assert len(messages) == 2  # Staff and student warning


def test_ruff_failure_is_reported(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    # Ruff writes nothing to stdout when it fails, which must not be mistaken
    # for a submission without any problems.
    mocker.patch(
        "tested.languages.python.ruff_linter.run_command",
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
    assert len(messages) == 2  # Staff and student warning
    assert len(updates.find_all("annotate-code")) == 0
