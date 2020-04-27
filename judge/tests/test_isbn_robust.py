"""
Test the judge using a Python exercise.
The original exercise is located at
https://github.ugent.be/pythia/programmeren/tree/master/opgaven/reeks07/ISBN
"""
import os
import shutil
import threading
from io import StringIO
from pathlib import Path
from typing import Optional

from tested.configs import DodonaConfig
from tested.main import run
from tests.manual_utils import merge, clean_working_directory


def read_config(exercise: str, work_dir: Path, options=None) -> DodonaConfig:
    """Read the configuration from stdout"""
    if options is None:
        options = {}
    return DodonaConfig(**merge({
        "memory_limit":         536870912,
        "time_limit":           threading.TIMEOUT_MAX,
        "programming_language": 'python',
        "natural_language":     'nl',
        "resources":            Path('../exercise/isbn/evaluation').resolve(),
        "source":               Path(f'tests/isbn/robust/{exercise}.py').resolve(),
        "judge":                Path('./src').resolve(),
        "workdir":              work_dir,
        "plan_name":            "plan.json",
    }, options))


def do_test(config: DodonaConfig, result: Optional[str] = None):
    if result is None:
        result = config.source.stem
    # Delete content in work dir
    clean_working_directory(config)

    actual = StringIO()
    run(config, actual)

    with open(f'tests/isbn/robust/{result}.dson', 'r') as f:
        expected_string = f.read()
    actual_string = actual.getvalue()
    print(actual_string)

    assert actual_string == expected_string


def test_runtime_error(tmp_path: Path):
    config = read_config("runtime-error", tmp_path)
    do_test(config)


def test_timout(tmp_path: Path):
    config = read_config("timeout", tmp_path, {
        "time_limit": 5
    })
    do_test(config)
