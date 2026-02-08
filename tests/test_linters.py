from pathlib import Path

import pytest

from tests.manual_utils import assert_valid_output, configuration, execute_config


def _get_config_options(language: str) -> list[dict]:
    return [
        {"options": {"language": {language: {"linter": True}}}},
        {"options": {"linter": True}},
    ]


@pytest.mark.parametrize("config", _get_config_options("c"))
def test_cppcheck_c(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "c",
        tmp_path,
        "one.tson",
        "correct-cppcheck",
        config,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("c"))
def test_cppcheck_c_bad_column(
    tmp_path: Path, config: dict, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "c",
        tmp_path,
        "one.tson",
        "bad-column-cppcheck",
        config,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("cpp"))
def test_cppcheck(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "cpp",
        tmp_path,
        "one.tson",
        "correct-cppcheck",
        config,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("java"))
def test_checkstyle(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "counter",
        "java",
        tmp_path,
        "plan.yaml",
        "solution-checkstyle",
        config,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("javascript"))
def test_eslint(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "counter",
        "javascript",
        tmp_path,
        "plan.yaml",
        "solution-eslint",
        config,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("typescript"))
def test_eslint_typescript(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "counter",
        "typescript",
        tmp_path,
        "plan.yaml",
        "solution-eslint",
        config,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    print(updates)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize(
    ("language", "config"),
    [
        ("haskell", _get_config_options("haskell")[0]),
        ("haskell", _get_config_options("haskell")[1]),
        ("runhaskell", _get_config_options("runhaskell")[0]),
        ("runhaskell", _get_config_options("runhaskell")[1]),
    ],
)
def test_hlint(
    language: str, config: dict, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        language,
        tmp_path,
        "one.tson",
        "correct_io",
        config,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("kotlin"))
def test_ktlint(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "counter", "kotlin", tmp_path, "plan.yaml", "solution", config
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("python"))
def test_pylint(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "counter",
        "python",
        tmp_path,
        "plan.yaml",
        "solution-pylint",
        config,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("bash"))
def test_shellcheck_wrong(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "echo", "bash", tmp_path, "one.tson", "wrong", config
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) > 0


@pytest.mark.parametrize("config", _get_config_options("bash"))
def test_shellcheck_warning(tmp_path: Path, config: dict, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "echo", "bash", tmp_path, "one.tson", "warning", config
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("annotate-code")) == 1
    [annotation] = updates.find_all("annotate-code")
    assert annotation["externalUrl"]
