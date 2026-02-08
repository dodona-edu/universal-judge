from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("language", ["haskell", "runhaskell"])
def test_hlint_correct(tmp_path: Path, pytestconfig: pytest.Config, language: str):
    conf = configuration(
        pytestconfig,
        "linter",
        language,
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


@pytest.mark.parametrize("language", ["haskell", "runhaskell"])
def test_hlint_warning(tmp_path: Path, pytestconfig: pytest.Config, language: str):
    conf = configuration(
        pytestconfig,
        "linter",
        language,
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
    assert "Redundant bracket" in annotations[0]["text"]


@pytest.mark.parametrize("language", ["haskell", "runhaskell"])
def test_hlint_multiline(tmp_path: Path, pytestconfig: pytest.Config, language: str):
    conf = configuration(
        pytestconfig,
        "linter",
        language,
        tmp_path,
        "plan.tson",
        "multiline",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1
    assert annotations[0]["rows"] == 3


@pytest.mark.parametrize("language", ["haskell", "runhaskell"])
def test_hlint_suggestion(tmp_path: Path, pytestconfig: pytest.Config, language: str):
    conf = configuration(
        pytestconfig,
        "linter",
        language,
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "haskell": {"linter": True, "hlint_config": "suggest_hlint.yml"},
                    "runhaskell": {"linter": True, "hlint_config": "suggest_hlint.yml"},
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1
    assert annotations[0]["type"] == Severity.INFO


@pytest.mark.parametrize("language", ["haskell", "runhaskell"])
def test_hlint_custom_config(
    tmp_path: Path, pytestconfig: pytest.Config, language: str
):
    conf = configuration(
        pytestconfig,
        "linter",
        language,
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "haskell": {"linter": True, "hlint_config": "custom_hlint.yml"},
                    "runhaskell": {"linter": True, "hlint_config": "custom_hlint.yml"},
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 0


def test_hlint_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.haskell.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="invalid json", stderr="", exit=0, timeout=False, memory=False
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "haskell",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 2  # Staff and student warning
