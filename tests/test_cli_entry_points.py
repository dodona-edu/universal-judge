import json
import subprocess
import sys
from pathlib import Path

import pytest

from tested.parsing import get_converter
from tests.manual_utils import assert_valid_output, configuration

REPO_ROOT = Path(__file__).parent.parent

DSL = """\
- tab: "Test"
  testcases:
    - stdin: "Hallo"
      stdout: "Hallo"
"""

PROBLEM = '{{ function("this_is_a_function_name") }}'


def run_module(module: str, *args: str, stdin: str = "") -> subprocess.CompletedProcess:
    return subprocess.run(
        [sys.executable, "-m", module, *args],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        input=stdin,
        check=False,
    )


def test_dsl_stdin_to_stdout():
    result = run_module("tested.dsl", stdin=DSL)

    assert result.returncode == 0, result.stderr
    assert "tabs" in json.loads(result.stdout)


def test_dsl_files(tmp_path: Path):
    source = tmp_path / "suite.yaml"
    source.write_text(DSL)
    output = tmp_path / "suite.json"

    result = run_module("tested.dsl", "-i", str(source), "-o", str(output))
    expected = run_module("tested.dsl", stdin=DSL)

    assert result.returncode == 0, result.stderr
    assert result.stdout == ""
    assert output.read_text() == expected.stdout
    assert "tabs" in json.loads(output.read_text())


@pytest.mark.parametrize(
    ("language", "expected"),
    [("python", "this_is_a_function_name"), ("java", "thisIsAFunctionName")],
)
def test_descriptions_stdin_to_stdout(language: str, expected: str):
    result = run_module("tested.descriptions", "-l", language, stdin=PROBLEM)

    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{expected}\n"


def test_descriptions_files(tmp_path: Path):
    source = tmp_path / "problem.md.jinja2"
    source.write_text(PROBLEM)
    output = tmp_path / "problem.md"

    result = run_module(
        "tested.descriptions", "-p", str(source), "-o", str(output), "-l", "java"
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout == ""
    assert output.read_text() == "thisIsAFunctionName\n"


@pytest.fixture
def judge_config(tmp_path: Path, pytestconfig: pytest.Config) -> str:
    work_dir = tmp_path / "work"
    work_dir.mkdir()
    conf = configuration(
        pytestconfig, "echo", "python", work_dir, "plan.yaml", "correct"
    )
    return json.dumps(get_converter().unstructure(conf), default=str)


@pytest.mark.python
def test_judge_stdin_to_stdout(judge_config: str, pytestconfig: pytest.Config):
    result = run_module("tested", stdin=judge_config)

    assert result.returncode == 0, result.stderr
    updates = assert_valid_output(result.stdout, pytestconfig)
    assert updates.find_status_enum() == ["correct", "correct", "correct"]
    assert updates.find_all("close-judgement")


@pytest.mark.python
def test_judge_files(judge_config: str, tmp_path: Path, pytestconfig: pytest.Config):
    config_file = tmp_path / "config.json"
    config_file.write_text(judge_config)
    output = tmp_path / "output.json"

    result = run_module("tested", "-c", str(config_file), "-o", str(output))

    assert result.returncode == 0, result.stderr
    assert result.stdout == ""
    updates = assert_valid_output(output.read_text(), pytestconfig)
    assert updates.find_status_enum() == ["correct", "correct", "correct"]
    assert updates.find_all("close-judgement")


@pytest.mark.parametrize(
    ("module", "input_flag", "extra_args"),
    [
        ("tested.dsl", "-i", ()),
        ("tested.descriptions", "-p", ("-l", "python")),
        ("tested", "-c", ()),
    ],
    ids=["dsl", "descriptions", "judge"],
)
def test_missing_input(
    module: str, input_flag: str, extra_args: tuple[str, ...], tmp_path: Path
):
    output = tmp_path / "output"

    result = run_module(
        module, input_flag, str(tmp_path / "missing"), "-o", str(output), *extra_args
    )

    assert result.returncode != 0
    assert "FileNotFoundError" in result.stderr
    assert not output.exists()
