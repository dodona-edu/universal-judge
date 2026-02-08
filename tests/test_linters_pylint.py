from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_pylint_correct(tmp_path: Path, pytestconfig: pytest.Config):
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


def test_pylint_error(tmp_path: Path, pytestconfig: pytest.Config):
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
    assert "undefined-variable" in annotations[0]["externalUrl"]


def test_pylint_warning(tmp_path: Path, pytestconfig: pytest.Config):
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


def test_pylint_convention(tmp_path: Path, pytestconfig: pytest.Config):
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
    assert "multiple-statements" in annotations[0]["externalUrl"]


def test_pylint_refactor(tmp_path: Path, pytestconfig: pytest.Config):
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


def test_pylint_custom_config(tmp_path: Path, pytestconfig: pytest.Config):
    # This config only enables W0612 (unused-variable)
    # Our warning.py has an unused import (W0611), which should be disabled.
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
                    "python": {"linter": True, "pylint_config": "pylint_config.rc"}
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 0


def test_pylint_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.python.linter.lint.Run",
        side_effect=Exception("Pylint crashed"),
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
