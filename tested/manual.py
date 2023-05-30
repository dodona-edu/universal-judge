"""
Run the judge manually from code. In this mode, the config is hardcoded into this
file, allowing rapid testing (and, most importantly, debugging).
"""
import logging
import os
import shutil
import sys
import time
from pathlib import Path

from tested.configs import DodonaConfig
from tested.main import run

exercise_dir = "/home/niko/Ontwikkeling/universal-judge/tests/exercises/echo"


def read_config() -> DodonaConfig:
    """Read the configuration from stdout"""
    # noinspection PyArgumentList
    return DodonaConfig(
        **{
            "memory_limit": 536870912,
            "time_limit": 60,
            "programming_language": "python",
            "natural_language": "nl",
            "resources": Path(exercise_dir, "evaluation"),
            "source": Path(exercise_dir, "solution/run-error.py"),
            "judge": Path("."),
            "workdir": Path("workdir"),
            "test_suite": "one.tson",
            "options": {
                "allow_fallback": False,
                "mode": "batch",
                "linter": False,
            },
        }
    )


if __name__ == "__main__":
    config = read_config()

    # Enable logging
    log = logging.getLogger()
    log.setLevel(logging.DEBUG)
    ch = logging.StreamHandler(stream=sys.stdout)
    formatter = logging.Formatter("%(name)s:%(levelname)s:%(message)s")
    ch.setFormatter(formatter)
    log.addHandler(ch)

    # Some modules are very verbose, hide those by default.
    logger = logging.getLogger("tested.judge.collector")
    logger.setLevel(logging.INFO)

    # Create workdir if needed.
    config.workdir.mkdir(exist_ok=True)

    # Delete content in work dir
    # noinspection PyTypeChecker
    for root, dirs, files in os.walk(config.workdir):
        for f in files:
            os.unlink(os.path.join(root, f))
        for d in dirs:
            shutil.rmtree(os.path.join(root, d), ignore_errors=True)

    start = time.time()
    run(config, sys.stdout)
    end = time.time()
    print()
    print(f"Judging took {end - start} seconds (real time)")
