from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_eslint_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "javascript",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


def test_eslint_warning(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "javascript",
        tmp_path,
        "plan.tson",
        "warning",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    assert len(annotations) == 2

    assert annotations[0]["type"] == Severity.WARNING
    assert annotations[0]["row"] == 0
    assert annotations[0]["rows"] == 1
    assert annotations[0]["column"] == 0
    assert annotations[0]["columns"] == 10
    assert "no-var" in annotations[0]["externalUrl"]

    assert annotations[1]["type"] == Severity.WARNING
    assert annotations[1]["row"] == 1
    assert annotations[1]["rows"] == 2
    assert annotations[1]["column"] == 14
    assert annotations[1]["columns"] == 0
    assert "semi" in annotations[1]["externalUrl"]


def test_eslint_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.javascript.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="invalid json", stderr="", exit=0, timeout=False, memory=False
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "javascript",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 2  # Staff and student warning


def test_eslint_multiline(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "javascript",
        tmp_path,
        "plan.tson",
        "multiline",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    assert len(annotations) == 2

    assert annotations[0]["type"] == Severity.WARNING
    assert "no-var" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 5
    assert annotations[0]["rows"] == 3
    assert annotations[0]["column"] == 0
    assert annotations[0]["columns"] == 3

    assert annotations[1]["type"] == Severity.WARNING
    assert "semi" in annotations[1]["externalUrl"]
    assert annotations[1]["row"] == 7
    assert annotations[1]["rows"] == 2
    assert annotations[1]["column"] == 3
    assert annotations[1]["columns"] == 0
