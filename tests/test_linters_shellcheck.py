from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_shellcheck_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


def test_shellcheck_warning(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "bash": {"linter": True, "shellcheck_config": "yes-unused-rc"}
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    assert len(annotations) == 1

    assert annotations[0]["type"] == Severity.WARNING
    assert "SC2034" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 0
    assert annotations[0]["rows"] == 1
    assert annotations[0]["column"] == 0
    assert annotations[0]["columns"] == 3


def test_shellcheck_error(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
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
    assert "SC1089" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 1
    assert annotations[0]["rows"] == 1
    assert annotations[0]["column"] == 0


def test_shellcheck_info(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
        tmp_path,
        "plan.tson",
        "info",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1

    assert annotations[0]["type"] == Severity.WARNING
    assert "SC2155" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 1
    assert annotations[0]["rows"] == 1
    assert annotations[0]["column"] == 7
    assert annotations[0]["columns"] == 3


def test_shellcheck_style(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
        tmp_path,
        "plan.tson",
        "style",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 3

    assert annotations[0]["type"] == Severity.WARNING
    assert "SC2034" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 1
    assert annotations[0]["rows"] == 1
    assert annotations[0]["column"] == 0
    assert annotations[0]["columns"] == 3

    assert annotations[1]["type"] == Severity.INFO
    assert "SC2006" in annotations[1]["externalUrl"]
    assert annotations[1]["row"] == 1
    assert annotations[1]["rows"] == 1
    assert annotations[1]["column"] == 4
    assert annotations[1]["columns"] == 10

    assert annotations[2]["type"] == Severity.INFO
    assert "SC2116" in annotations[2]["externalUrl"]
    assert annotations[2]["row"] == 1
    assert annotations[2]["rows"] == 1
    assert annotations[2]["column"] == 4
    assert annotations[2]["columns"] == 10


def test_shellcheck_multiline(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
        tmp_path,
        "plan.tson",
        "multiline",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1

    assert annotations[0]["type"] == Severity.INFO
    assert "SC2086" in annotations[0]["externalUrl"]
    assert annotations[0]["row"] == 1
    assert annotations[0]["rows"] == 2
    assert annotations[0]["column"] == 5
    assert annotations[0]["columns"] == 7


def test_shellcheck_config(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "bash": {"linter": True, "shellcheck_config": "no-unused-rc"}
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    # SC2034 should be disabled by the shellcheckrc
    assert len(annotations) == 0


def test_shellcheck_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.bash.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="invalid json", stderr="", exit=0, timeout=False, memory=False
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 2  # Staff and student warning
