"""
Run the judge manually from code. In this mode, the config is hardcoded into this
file, allowing rapid testing (and, most importantly, debugging).
"""

import logging
import sys
import time
from pathlib import Path

from tested.cli import create_and_populate_workdir
from tested.configs import DodonaConfig, Options
from tested.main import run
from tested.testsuite import SupportedLanguage

exercise_dir = "/IdeaProjects/universal-judge/tests/exercises/echo-function"


def read_config() -> DodonaConfig:
    """Read the configuration from stdout"""
    return DodonaConfig(
        memory_limit=536870912,
        time_limit=60,
        programming_language=SupportedLanguage("cpp"),
        natural_language="nl",
        resources=Path(exercise_dir, "evaluation"),
        source=Path(exercise_dir, "solution/correct.cpp"),
        judge=Path("."),
        workdir=Path("workdir"),
        test_suite="one-escape.tson",
        options=Options(
            linter=False,
        ),
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
    logger = logging.getLogger("tested.parsing")
    logger.setLevel(logging.INFO)

    create_and_populate_workdir(config)

    start = time.time()
    run(config, sys.stdout)
    end = time.time()
    print()
    print(f"Judging took {end - start} seconds (real time)")
