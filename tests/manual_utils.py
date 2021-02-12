import json
import re
import threading
from io import StringIO
from pathlib import Path
from typing import List

import pytest
from _pytest.config import Config
from jsonschema import validate

from tested.configs import DodonaConfig
from tested.languages import get_language
from tested.main import run


def merge(a: dict, b: dict, path=None) -> dict:
    """merges b into a"""
    if path is None: path = []
    for key in b:
        if key in a:
            if isinstance(a[key], dict) and isinstance(b[key], dict):
                merge(a[key], b[key], path + [str(key)])
            elif a[key] == b[key]:
                pass  # same leaf value
            else:
                a[key] = b[key]
        else:
            a[key] = b[key]
    return a


def split_output(output: str) -> List[str]:
    return re.compile(r"(?<=})\s*(?={)").split(output)


class CommandDict(list):

    def find_all(self, command: str) -> List[dict]:
        return [x for x in self if x["command"] == command]

    def find_next(self, command: str) -> dict:
        return next(x for x in self if x["command"] == command)

    def find_status_enum(self) -> List[str]:
        commands = [x for x in self if x["command"].startswith("close-") or x["command"] == "escalate-status"]
        return [x["status"]["enum"] for x in commands if "status" in x]


def assert_valid_output(output: str, config: Config) -> CommandDict:
    # noinspection PyTypeChecker
    with open(Path(config.rootdir) / "tests/partial_output.json", "r") as f:
        schema = json.load(f)

    updates = CommandDict()
    # Every update should be valid.
    for update in split_output(output):
        update_object = json.loads(update)
        validate(update_object, schema)
        updates.append(update_object)
    assert updates
    return updates


def configuration(config, exercise: str, language: str, work_dir: Path,
                  plan: str = "plan.json", solution: str = "solution",
                  options=None) -> DodonaConfig:
    """Create a config."""
    # Get the file extension for this language.
    ext = get_language(language).extension_file()
    if options is None:
        options = {}
    exercise_dir = Path(config.rootdir) / "exercise"
    ep = f'{exercise_dir}/{exercise}'
    return DodonaConfig(**merge({
        "memory_limit":         536870912,
        "time_limit":           3600,  # One hour
        "programming_language": language,
        "natural_language":     'nl',
        "resources":            Path(f'{ep}/evaluation'),
        "source":               Path(f'{ep}/solution/{solution}.{ext}'),
        "judge":                Path(f'{config.rootdir}'),
        "workdir":              work_dir,
        "plan_name":            plan,
    }, options))


def execute_config(config: DodonaConfig) -> str:
    actual = StringIO()
    run(config, actual)
    result = actual.getvalue()
    print(result)
    return result


mark_haskell = pytest.mark.haskell
