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
    sorted_annotations = sorted(annotations, key=lambda a: a["externalUrl"])

    assert sorted_annotations[0]["type"] == Severity.INFO
    assert "SC2006" in sorted_annotations[0]["externalUrl"]

    assert sorted_annotations[1]["type"] == Severity.WARNING
    assert "SC2034" in sorted_annotations[1]["externalUrl"]

    assert sorted_annotations[2]["type"] == Severity.INFO
    assert "SC2116" in sorted_annotations[2]["externalUrl"]


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
    annotation = annotations[0]
    assert "SC2086" in annotation["externalUrl"]
    assert annotation["rows"] == 2
    assert annotation["columns"] == 7


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


def test_shellcheck_dialect(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "bash",
        tmp_path,
        "plan.tson",
        "sh_dialect",
        {
            "options": {
                "language": {"bash": {"linter": True, "shellcheck_language": "sh"}}
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 2
    assert any("SC3010" in a["externalUrl"] for a in annotations)
    assert any("SC3014" in a["externalUrl"] for a in annotations)


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
