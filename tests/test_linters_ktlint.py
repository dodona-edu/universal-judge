from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_ktlint_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "kotlin",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


def test_ktlint_warning(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "kotlin",
        tmp_path,
        "plan.tson",
        "warning",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    # standard:no-wildcard-imports and standard:no-trailing-spaces
    # standard:filename should be disabled by default editorconfig
    assert len(annotations) == 2
    assert all(a["type"] == Severity.INFO for a in annotations)
    assert any("no-wildcard-imports" in a["text"] for a in annotations)
    assert any("no-trailing-spaces" in a["text"] for a in annotations)
    assert annotations[0]["row"] == 0
    assert annotations[1]["row"] == 4


def test_ktlint_custom_config(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "kotlin",
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "kotlin": {
                        "linter": True,
                        "editorconfig": "ktlint_config.editorconfig",
                    }
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    # no-wildcard-imports disabled, but standard:filename is enabled (because we override editorconfig)
    # and no-trailing-spaces is still there.
    assert len(annotations) == 2
    assert any("filename" in a["text"] for a in annotations)
    assert any("no-trailing-spaces" in a["text"] for a in annotations)
    assert not any("no-wildcard-imports" in a["text"] for a in annotations)


def test_ktlint_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.kotlin.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="invalid json", stderr="", exit=0, timeout=False, memory=False
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "kotlin",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 3
