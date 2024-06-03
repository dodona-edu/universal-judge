"""
This test will execute the sample solution for every JavaScript exercise to see if
they pass.
"""

import json
import os
from pathlib import Path

import pytest

from tests.manual_utils import (
    assert_valid_output,
    execute_config,
    exercise_configuration,
)

LOCAL_REPOSITORY: str | None = os.getenv("EXERCISE_REPO")

# Exercises to exclude, using the exercise folder name.
BLACKLIST = ["mad libs", "bifidcodering"]


def prepare_repository() -> Path:
    """
    Download the repository to a temporary folder or use the local repository.
    :return: Path to the repository.
    """

    if LOCAL_REPOSITORY:
        local_path = Path(LOCAL_REPOSITORY)
        if not local_path.is_dir():
            raise Exception(
                f"""
                Local repository not found. LOCAL_REPOSITORY is set to {LOCAL_REPOSITORY},
                but this is not a valid directory.
            """
            )
        return local_path

    raise Exception(
        "Path to local repository not set. Set the EXERCISE_REPO environment variable."
    )


def find_all_exercises(root_directory: Path) -> list[Path]:
    all_exercises = []
    for reeks_path in root_directory.glob("*"):
        if reeks_path.is_dir():
            for oefening_path in reeks_path.glob("*"):
                if oefening_path.is_dir():
                    config_file = oefening_path / "config.json"
                    if not config_file.exists():
                        print(f"Skipping {oefening_path}, not an exercise.")
                        continue
                    exercise_data = json.loads(config_file.read_text())
                    if exercise_data.get("evaluation", {}).get("handler") is not None:
                        print(f"Skipping {oefening_path}, not TESTed.")
                        continue
                    if oefening_path.name in BLACKLIST:
                        print(f"Skipping {oefening_path}, blacklisted.")
                        continue
                    all_exercises.append(oefening_path)
    return all_exercises


def get_exercises() -> list[Path]:
    # Get path to the repo.
    repository = prepare_repository()

    if not repository:
        return []

    # Get all exercises in the repository.
    root_directory = repository / "reeksen"
    return find_all_exercises(root_directory)


ALL_EXERCISES = get_exercises()


@pytest.mark.parametrize("exercise", ALL_EXERCISES, ids=lambda ex: ex.name)
def test_javascript_exercise(exercise: Path, tmp_path: Path, pytestconfig, snapshot):
    conf = exercise_configuration(
        pytestconfig,
        exercise,
        "javascript",
        tmp_path,
        "suite.yaml",
        "solution.nl",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    status = updates.find_status_enum()
    assert set(status) == {"correct"}
    assert status == snapshot
