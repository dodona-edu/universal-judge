import json
import shutil
from io import StringIO
from pathlib import Path

import pytest
from jsonschema import validate

from tested.cli import CommandDict, split_output
from tested.configs import DodonaConfig
from tested.languages import get_language
from tested.main import run
from tested.parsing import get_converter
from tested.utils import recursive_dict_merge


def assert_valid_output(output: str, config: pytest.Config) -> CommandDict:
    with open(config.rootpath / "tests/partial_output.json", "r") as f:
        schema = json.load(f)

    updates = CommandDict()
    # Every update should be valid.
    for update in split_output(output):
        update_object = json.loads(update)
        validate(update_object, schema)
        updates.append(update_object)
    assert updates
    return updates


def configuration(
    config: pytest.Config,
    exercise: str,
    language: str,
    work_dir: Path,
    suite: str = "plan.json",
    solution: str = "solution",
    options: dict | None = None,
) -> DodonaConfig:
    exercise_dir = config.rootpath / "tests" / "exercises"
    ep = exercise_dir / exercise
    return exercise_configuration(
        config, ep, language, work_dir, suite, solution, options
    )


def exercise_configuration(
    config: pytest.Config,
    exercise_directory: Path,
    language: str,
    work_dir: Path,
    suite: str,
    solution: str,
    options=None,
) -> DodonaConfig:
    # Get the file extension for this language.
    ext = get_language(None, language).file_extension()
    if options is None:
        options = {}
    option_dict = recursive_dict_merge(
        {
            "memory_limit": 536870912,
            "time_limit": 3600,  # One hour
            "programming_language": language,
            "natural_language": "nl",
            "resources": exercise_directory / "evaluation",
            "source": exercise_directory / "solution" / f"{solution}.{ext}",
            "judge": config.rootpath,
            "workdir": work_dir,
            "test_suite": suite,
            "options": {"linter": False},
        },
        options,
    )

    # Check if we need to populate the workdir.
    if (workdir_files := exercise_directory / "workdir").is_dir():
        shutil.copytree(workdir_files, work_dir, dirs_exist_ok=True)

    return get_converter().structure(option_dict, DodonaConfig)


def execute_config(config: DodonaConfig) -> str:
    actual = StringIO()
    run(config, actual)
    result = actual.getvalue()
    print(result)
    return result
