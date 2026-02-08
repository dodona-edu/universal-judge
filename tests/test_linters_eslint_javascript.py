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
    assert all(a["type"] == Severity.WARNING for a in annotations)
    assert any("no-var" in a["externalUrl"] for a in annotations)
    assert any("semi" in a["externalUrl"] for a in annotations)


def test_eslint_custom_config(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "javascript",
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "javascript": {"linter": True, "eslint_config": "eslint_config.yml"}
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    assert len(annotations) == 1
    assert annotations[0]["type"] == Severity.ERROR
    assert "semi" in annotations[0]["externalUrl"]


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
    no_var = next(a for a in annotations if "no-var" in a["externalUrl"])
    assert no_var["row"] == 5
    assert no_var["rows"] == 2
