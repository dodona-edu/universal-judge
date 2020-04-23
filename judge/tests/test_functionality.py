"""
Testcases for specific functionality. These tests ensure the judge works.
These tests are a compromise: they don't test the complete output, but this does
make them usable for multiple languages.

The configure which languages are tested, modify the `conftest.py` file.

Running the tests should happen in with the root directory (the one with src/ and
tests/) as the working directory.
"""
import threading
from io import StringIO
from pathlib import Path
from typing import List

import pytest
from _pytest.config import Config

from tested.configs import DodonaConfig
from tested.languages import get_language
from tested.main import run
from tests.manual_utils import merge, assert_valid_output


def configuration(config: Config, exercise: str, language: str, work_dir: Path,
                  plan: str = "plan.json", solution: str = "solution",
                  options=None) -> DodonaConfig:
    """Create a config."""
    # Get the file extension for this language.
    ext = get_language(language).file_extension()
    if options is None:
        options = {}
    ep = f'{config.rootdir}/tests/cases/{exercise}'
    return DodonaConfig(**merge({
        "memory_limit":         536870912,
        "time_limit":           threading.TIMEOUT_MAX,
        "programming_language": language,
        "natural_language":     'nl',
        "resources":            Path(f'{ep}/evaluation'),
        "source":               Path(f'{ep}/solution/{solution}.{ext}'),
        "judge":                Path(f'{config.rootdir}/src'),
        "workdir":              work_dir,
        "plan_name":            plan,
    }, options))


def execute_config(config: DodonaConfig) -> str:
    actual = StringIO()
    run(config, actual)
    return actual.getvalue()


def test_io_exercise(language: str, tmp_path: Path, pytestconfig: Config):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "simple.json", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_simple_programmed_eval(language: str, tmp_path: Path, pytestconfig: Config):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "simple-programmed.json", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_simple_programmed_eval_wrong(language: str, tmp_path: Path, pytestconfig: Config):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "simple-programmed-wrong.json", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


def test_io_function_exercise(language: str, tmp_path: Path, pytestconfig: Config):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "simple.json", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", ["python", "java", "haskell"])
def test_language_evaluator_exception(lang: str, tmp_path: Path, pytestconfig: Config):
    conf = configuration(pytestconfig, "division", lang, tmp_path, "plan.json", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
