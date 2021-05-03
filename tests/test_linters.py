from pathlib import Path

import pytest

from tests.manual_utils import configuration, execute_config, assert_valid_output, mark_haskell


def _get_config_options(language: str):
    return [{
        "options": {
            "language": {
                language: {
                    "linter": True
                }
            }
        }
    }, {
        "options": {
            "linter": True
        }
    }]


@pytest.mark.linter
@pytest.mark.parametrize("config", _get_config_options("c"))
def test_cppcheck(tmp_path: Path, config, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", "c", tmp_path, "one.tson", "correct-cppcheck", config)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
@pytest.mark.parametrize("config", _get_config_options("java"))
def test_checkstyle(tmp_path: Path, config, pytestconfig):
    conf = configuration(pytestconfig, "counter", "java", tmp_path, "plan.yaml", "solution-checkstyle", config)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
@pytest.mark.parametrize("config", _get_config_options("javascript"))
def test_eslint(tmp_path: Path, config, pytestconfig):
    conf = configuration(pytestconfig, "counter", "javascript", tmp_path, "plan.yaml", "solution-eslint", config)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
@mark_haskell
@pytest.mark.parametrize(("language", "config"), [("haskell", _get_config_options("haskell")[0]),
                                                  ("haskell", _get_config_options("haskell")[1]),
                                                  ("runhaskell", _get_config_options("runhaskell")[0]),
                                                  ("runhaskell", _get_config_options("runhaskell")[1])])
def test_hlint(language: str, config, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct_io", config)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
@pytest.mark.ktlint
@pytest.mark.parametrize("config", _get_config_options("kotlin"))
def test_ktlint(tmp_path: Path, config, pytestconfig):
    conf = configuration(pytestconfig, "counter", "kotlin", tmp_path, "plan.yaml", "solution", config)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
@pytest.mark.parametrize("config", _get_config_options("python"))
def test_pylint(tmp_path: Path, config, pytestconfig):
    conf = configuration(pytestconfig, "counter", "python", tmp_path, "plan.yaml", "solution-pylint", config)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.linter
@pytest.mark.parametrize("config", _get_config_options("bash"))
def test_shellcheck(tmp_path: Path, config, pytestconfig):
    conf = configuration(pytestconfig, "echo", "bash", tmp_path, "one.tson", "wrong", config)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0
