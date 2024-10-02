"""
Tests for specific aspects of certain language implementations.
"""

import shutil
import sys
from pathlib import Path

import pytest

from tested.configs import create_bundle
from tested.datatypes import BasicBooleanTypes, BasicNumericTypes, BasicStringTypes
from tested.languages.conventionalize import submission_name
from tested.languages.generation import generate_statement
from tested.serialisation import (
    BooleanType,
    FunctionCall,
    FunctionType,
    NumberType,
    StringType,
)
from tested.testsuite import Suite
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_javascript_vanilla_object(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "javascript",
        tmp_path,
        "javascript-object.yaml",
        "javascript-object",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_python_input_prompt_is_ignored(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "one.tson",
        "input-prompt",
    )

    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_haskell_function_arguments_without_brackets(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "", "haskell", tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)

    statement = FunctionCall(
        type=FunctionType.FUNCTION,
        name="test",
        namespace=None,
        arguments=[
            NumberType(type=BasicNumericTypes.REAL, data=5.5),
            StringType(type=BasicStringTypes.TEXT, data="hallo"),
            BooleanType(type=BasicBooleanTypes.BOOLEAN, data=True),
        ],
    )

    result = generate_statement(bundle, statement)
    assert (
        result == f'{submission_name(bundle.language)}.test 5.5 "hallo" True'
    )


def test_javascript_exception_correct(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 0


def test_javascript_exception_correct_temp(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "correct-temp",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 0


def test_javascript_exception_wrong(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "wrong",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


def test_javascript_exception_wrong_null(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "wrong-null",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 0


def test_javascript_exception_missing_message(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "js-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "wrong-message",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("exercise", ["echo-function-file-input", "echo-function"])
def test_javascript_async(exercise: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, exercise, "javascript", tmp_path, "one.tson", "correct-async"
    )
    workdir = Path(conf.resources).parent / "workdir"
    if workdir.exists():
        shutil.copytree(workdir, tmp_path, dirs_exist_ok=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
