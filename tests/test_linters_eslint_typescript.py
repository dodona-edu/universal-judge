from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_eslint_typescript_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "typescript",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


def test_eslint_typescript_warning(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "typescript",
        tmp_path,
        "plan.tson",
        "warning",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    # Expectations based on manual run:
    # 1. no-var (WARNING)
    # 2. @typescript-eslint/no-unused-vars (ERROR)
    # 3. semi (WARNING)
    assert len(annotations) == 3
    assert any(
        a["type"] == Severity.WARNING and "no-var" in a["externalUrl"]
        for a in annotations
    )
    assert any(
        a["type"] == Severity.ERROR and "no-unused-vars" in a["externalUrl"]
        for a in annotations
    )
    assert any(
        a["type"] == Severity.WARNING and "semi" in a["externalUrl"]
        for a in annotations
    )


def test_eslint_typescript_custom_config(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "typescript",
        tmp_path,
        "plan.tson",
        "warning",
        {
            "options": {
                "language": {
                    "typescript": {
                        "linter": True,
                        "eslint_config": "eslint_ts_config.yml",
                    }
                }
            }
        },
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    # Custom config has semi: off and no-var: error
    # Also has @typescript-eslint/recommended which includes no-unused-vars (error)
    assert len(annotations) == 2
    assert any(
        a["type"] == Severity.ERROR and "no-var" in a["externalUrl"]
        for a in annotations
    )
    assert any(
        a["type"] == Severity.ERROR and "no-unused-vars" in a["externalUrl"]
        for a in annotations
    )


def test_eslint_typescript_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.typescript.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="invalid json", stderr="", exit=0, timeout=False, memory=False
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "typescript",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    # There are 2 linter error messages and 1 compilation error message because of the mock
    assert len(messages) == 3


def test_eslint_typescript_multiline(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "typescript",
        tmp_path,
        "plan.tson",
        "multiline",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")

    # no-var should span lines 1 to 4
    # there is also a no-unused-vars for 'y'
    assert len(annotations) == 2
    no_var = next(a for a in annotations if "no-var" in a["externalUrl"])
    assert no_var["row"] == 0
    assert no_var["rows"] == 3
