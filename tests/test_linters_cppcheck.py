from pathlib import Path

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Severity
from tested.judge.utils import BaseExecutionResult
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.c
def test_cppcheck_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "c",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


@pytest.mark.c
def test_cppcheck_error(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "c",
        tmp_path,
        "plan.tson",
        "error",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    # cppcheck flags arrayIndexOutOfBounds as error
    # It also flags unreadVariable as style for the same line in my tests
    assert len(annotations) == 2

    assert annotations[0]["type"] == Severity.ERROR
    assert annotations[0]["row"] == 2
    assert annotations[0]["column"] == 5

    assert annotations[1]["type"] == Severity.INFO
    assert annotations[1]["row"] == 2
    assert annotations[1]["column"] == 9


@pytest.mark.c
def test_cppcheck_warning(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "c",
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
    assert annotations[0]["row"] == 3
    assert annotations[0]["column"] == 4


@pytest.mark.c
def test_cppcheck_style(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "c",
        tmp_path,
        "plan.tson",
        "style",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1

    assert annotations[0]["type"] == Severity.INFO
    assert annotations[0]["row"] == 1
    assert annotations[0]["column"] == 10


@pytest.mark.c
def test_cppcheck_branches(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "c",
        tmp_path,
        "plan.tson",
        "branches",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    # Normal check level stops after 4 always-true conditions and adds a note
    assert len([a for a in annotations if a["text"].startswith("Condition")]) > 4
    assert not any("Limiting analysis" in a["text"] for a in annotations)


@pytest.mark.c
def test_cppcheck_bad_output(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.c.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="", stderr="invalid xml", exit=0, timeout=False, memory=False
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "c",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    messages = updates.find_all("append-message")
    assert len(messages) == 2  # Educator and student warning


CPPCHECK_FILE_LEVEL_XML = """<?xml version="1.0" encoding="UTF-8"?>
<results version="2">
    <cppcheck version="2.17.1"/>
    <errors>
        <error id="internalError" severity="error" msg="Bailing out from analysis: Checking file failed" verbose="Bailing out from analysis: Checking file failed" file0="submission.c">
            <location file="submission.c" line="0" column="0"/>
        </error>
    </errors>
</results>
"""


@pytest.mark.c
def test_cppcheck_file_level_message(
    tmp_path: Path, pytestconfig: pytest.Config, mocker: MockerFixture
):
    mocker.patch(
        "tested.languages.c.linter.run_command",
        return_value=BaseExecutionResult(
            stdout="",
            stderr=CPPCHECK_FILE_LEVEL_XML,
            exit=0,
            timeout=False,
            memory=False,
        ),
    )
    conf = configuration(
        pytestconfig,
        "linter",
        "c",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 1
    assert annotations[0]["row"] == 0


@pytest.mark.cpp
def test_cppcheck_cpp_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "cpp",
        tmp_path,
        "plan.tson",
        "correct",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 0


@pytest.mark.cpp
def test_cppcheck_cpp_error(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "linter",
        "cpp",
        tmp_path,
        "plan.tson",
        "error",
        {"options": {"linter": True}},
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    annotations = updates.find_all("annotate-code")
    assert len(annotations) == 2

    assert annotations[0]["type"] == Severity.ERROR
    assert annotations[0]["row"] == 2
    assert annotations[0]["column"] == 5

    assert annotations[1]["type"] == Severity.INFO
    assert annotations[1]["row"] == 2
    assert annotations[1]["column"] == 9
