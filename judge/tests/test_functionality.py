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

import pytest

from tested.configs import DodonaConfig
from tested.languages import get_language
from tested.main import run
from tests.manual_utils import merge, assert_valid_output

ALL_LANGUAGES = ["python", "java", "haskell", "c"]


def configuration(config, exercise: str, language: str, work_dir: Path,
                  plan: str = "plan.json", solution: str = "solution",
                  options=None) -> DodonaConfig:
    """Create a config."""
    # Get the file extension for this language.
    ext = get_language(language).p_extension_file()
    if options is None:
        options = {}
    exercise_dir = Path(config.rootdir).parent / "exercise"
    ep = f'{exercise_dir}/{exercise}'
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


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_simple_programmed_eval(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one-programmed-correct.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_simple_programmed_eval_wrong(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo", language, tmp_path, "one-programmed-wrong.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("language", ALL_LANGUAGES)
def test_io_function_exercise(language: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "echo-function", language, tmp_path, "one.tson", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", ["python", "java", "haskell"])
def test_language_evaluator_exception(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "division", lang, tmp_path, "plan.json", "correct")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("lang", ["python"])
def test_assignment_and_use_in_expression(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "one-with-assignment.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Assert that the empty context testcase is not shown, while the assignment
    # and expression testcase are shown.
    assert len(updates.find_all("start-testcase")) == 2
    # Assert the only one test was executed.
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("start-test")) == 1


@pytest.mark.parametrize("lang", ["python"])
def test_crashing_assignment_with_before(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "one-with-crashing-assignment.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    # Only the assignment was started.
    assert len(updates.find_all("start-testcase")) == 1
    assert updates.find_status_enum() == ["wrong"]
    # Assert the exception is included.
    assert updates.find_next("start-test")["channel"] == "exception"


@pytest.mark.parametrize("lang", ["haskell", "c"])
def test_heterogeneous_arguments_are_detected(lang: str, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "full.tson", "solution")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 0
    assert updates.find_status_enum() == ["internal error"]


@pytest.mark.slow
@pytest.mark.parametrize("lang", ["python"])
def test_full_isbn(lang: str, tmp_path: Path, pytestconfig):
    config_ = {
        "options": {
            "parallel": True
        }
    }
    conf = configuration(pytestconfig, "isbn", lang, tmp_path, "full.tson", "solution", options=config_)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-testcase")) == 150
    assert updates.find_status_enum() == ["correct"] * 100
