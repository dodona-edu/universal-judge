from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_checkstyle_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "java",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


def test_checkstyle_warning(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "java",
        tmp_path,
        "plan.tson",
        "warning",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 3
    for annotation in annotations:
        assert annotation["type"] == Severity.ERROR
        assert "checkstyle.sourceforge.io" in annotation["externalUrl"]


def test_checkstyle_custom_config(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "java",
        tmp_path,
        "plan.tson",
        "correct",
        {
            "options": {
                "language": {
                    "java": {
                        "linter": True,
                        "checkstyle_config": "checkstyle_config.xml",
                    }
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 0


def test_checkstyle_custom_config_violation(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "linter",
        "java",
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "java": {
                        "linter": True,
                        "checkstyle_config": "checkstyle_config.xml",
                    }
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 2


def test_checkstyle_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.java.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="invalid xml", stderr="", exit=0, timeout=False, memory=False
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "java",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 2  # Staff and student warning
