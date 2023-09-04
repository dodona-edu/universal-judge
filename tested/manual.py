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

from tested.configs import DodonaConfig, Options
from tested.main import run
from tested.testsuite import ExecutionMode, SupportedLanguage

exercise_dir = "/home/niko/Ontwikkeling/CG-Dodona/reeks09/rebussen oplossen/"


def read_config() -> DodonaConfig:
    """Read the configuration from stdout"""
    return DodonaConfig(
        memory_limit=536870912,
        time_limit=60,
        programming_language=SupportedLanguage("bash"),
        natural_language="nl",
        resources=Path(exercise_dir, "evaluation"),
        source=Path("test.sh"),
        judge=Path("."),
        workdir=Path("workdir"),
        test_suite="tests.yaml",
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

    # Create workdir if needed.
    config.workdir.mkdir(exist_ok=True)

    # Delete content in work dir
    for root, dirs, files in os.walk(config.workdir):
        for f in files:
            os.unlink(os.path.join(root, f))
        for d in dirs:
            shutil.rmtree(os.path.join(root, d), ignore_errors=True)

        # Copy existing files to workdir if needed.
    if Path(exercise_dir, "workdir").is_dir():
        shutil.copytree(
            Path(exercise_dir, "workdir"), config.workdir, dirs_exist_ok=True
        )

    start = time.time()
    run(config, sys.stdout)
    end = time.time()
    print()
    print(f"Judging took {end - start} seconds (real time)")
