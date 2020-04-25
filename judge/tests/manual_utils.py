import os
import re
import shutil
from pathlib import Path
from typing import List

from _pytest.config import Config
from jsonschema import validate
import json



from tested.configs import DodonaConfig


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


def clean_working_directory(config: DodonaConfig):
    # noinspection PyTypeChecker
    for root, dirs, files in os.walk(config.workdir):
        for f in files:
            os.unlink(os.path.join(root, f))
        for d in dirs:
            shutil.rmtree(os.path.join(root, d), ignore_errors=True)


def split_output(output: str) -> List[str]:
    return re.compile(r"(?<=})\s*(?={)").split(output)


class CommandDict(list):

    def find_all(self, command: str) -> List[dict]:
        return [x for x in self if x["command"] == command]

    def find_next(self, command: str) -> dict:
        return next(x for x in self if x["command"] == command)

    def find_status_enum(self) -> List[str]:
        commands = [x for x in self if x["command"].startswith("close-")]
        return [x["status"]["enum"] for x in commands if "status" in x]


def assert_valid_output(output: str, config: Config) -> CommandDict:
    # noinspection PyTypeChecker
    with open(Path(config.rootdir) / "tests/cases/partial_output.json", "r") as f:
        schema = json.load(f)

    updates = CommandDict()
    # Every update should be valid.
    for update in split_output(output):
        update_object = json.loads(update)
        validate(update_object, schema)
        updates.append(update_object)
    assert updates
    return updates
