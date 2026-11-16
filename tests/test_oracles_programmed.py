"""
Tests for programmed oracles (also known as custom check functions).
"""

import json
import sys
from pathlib import Path

import pytest
from pytest_mock import MockerFixture

import tested.oracles.programmed
from tested.configs import create_bundle
from tested.datatypes import BasicStringTypes
from tested.dodona import Status
from tested.internationalization import get_i18n_string
from tested.oracles.common import OracleConfig
from tested.oracles.programmed import evaluate as evaluate_programmed
from tested.serialisation import StringType
from tested.testsuite import (
    CustomCheckOracle,
    EvaluationFunction,
    Suite,
    ValueOutputChannel,
)
from tests.language_markers import ALL_LANGUAGES, OBJECT_LANGUAGES
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_custom_check_function_stdout(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo",
        language,
        tmp_path,
        "one-programmed-correct.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_custom_check_function_stdout_wrong_result(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo", "python", tmp_path, "one-programmed-wrong.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_custom_check_function_return(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "programmed.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message"))


def test_custom_check_function_runtime_crash(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "programmed_crash.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]
    assert len(updates.find_all("append-message")) == 4


def test_custom_check_function_can_use_sys_without_import(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "programmed_sys.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ["javascript", "typescript"])
def test_custom_check_function_receives_set_of_arrays(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "programmed_set_of_arrays.yaml",
        "set-of-arrays",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_custom_check_function_non_text_readable_values(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "programmed_non_text.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert updates.find_next("start-test")["expected"] == "5"
    assert updates.find_next("close-test")["generated"] == "True"


def test_custom_check_function_syntax_error(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "programmed_syntax_error.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]
    assert len(updates.find_all("append-message")) == 4


def test_missing_custom_check_function(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "python",
        tmp_path,
        "programmed_missing.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]
    assert len(updates.find_all("append-message")) == 4


@pytest.mark.parametrize("lang", OBJECT_LANGUAGES)
def test_custom_check_function_lotto_correct(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "lotto", lang, tmp_path, "one-programmed-python.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", OBJECT_LANGUAGES)
def test_custom_check_function_lotto_wrong(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "lotto", lang, tmp_path, "one-programmed-python.tson", "wrong"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


def test_custom_check_function_static_analysis_correct(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "lotto",
        "python",
        tmp_path,
        "one-programmed-analysis-correct.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 0


def test_custom_check_function_static_analysis_wrong(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "lotto",
        "python",
        tmp_path,
        "one-programmed-analysis-wrong.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


def test_custom_check_function_static_analysis_missing_language(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "lotto",
        "python",
        tmp_path,
        "one-programmed-analysis-without-language.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.parametrize("lang", OBJECT_LANGUAGES)
def test_custom_check_function_files(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "output-files-custom-oracle",
        lang,
        tmp_path,
        "suite.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", OBJECT_LANGUAGES)
def test_custom_check_function_files_wrong(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "output-files-custom-oracle",
        lang,
        tmp_path,
        "suite.yaml",
        "wrong",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


def test_custom_check_function_unencodable_return(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "unencodable-return", "python", tmp_path, "suite.yaml", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"] * 3
    messages = [m["message"] for m in updates.find_all("append-message")]
    assert messages == [get_i18n_string("oracles.programmed.unknown-value")] * 3


def _sequence_of(element):
    return {"type": "sequence", "data": [element]}


@pytest.mark.parametrize(
    ("make_actual", "status"),
    [
        (_sequence_of, Status.INTERNAL_ERROR),
        (
            lambda code: _sequence_of(
                {"type": "function", "name": "eval", "arguments": [code]}
            ),
            Status.INTERNAL_ERROR,
        ),
        (lambda code: {"type": "unknown", "data": code}, Status.WRONG),
        (
            lambda code: _sequence_of({"type": "unknown", "data": code}),
            Status.WRONG,
        ),
    ],
    ids=["identifier", "function-call", "unknown", "unknown-in-sequence"],
)
def test_custom_check_function_does_not_run_code_from_the_actual_value(
    make_actual,
    status: Status,
    tmp_path: Path,
    pytestconfig: pytest.Config,
    mocker: MockerFixture,
):
    marker = tmp_path / "marker"
    code = f"open({str(marker)!r}, 'w').write('x')"
    actual = json.dumps(make_actual(code))

    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, Suite())
    config = OracleConfig(bundle=bundle, options={}, context_dir=tmp_path)
    channel = ValueOutputChannel(
        value=StringType(type=BasicStringTypes.TEXT, data="unused"),
        oracle=CustomCheckOracle(
            function=EvaluationFunction(Path("evaluator.py"), "evaluate_value")
        ),
    )
    run_oracle = mocker.spy(tested.oracles.programmed, "_evaluate_programmed")

    result = evaluate_programmed(config, channel, actual)

    assert not marker.exists()
    run_oracle.assert_not_called()
    assert result.result.enum == status
