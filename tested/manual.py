"""
Run the judge manually from code. In this mode, the configs is hardcoded into this
file, allowing rapid testing (and, most importantly, debugging).
"""
import logging
import os
import sys

import shutil
import time
from pathlib import Path

from .configs import DodonaConfig
from .main import run
from tested import internal_timings

exercise_dir = "exercise/echo"


def read_config() -> DodonaConfig:
    """Read the configuration from stdout"""
    return DodonaConfig(**{
        "memory_limit":         536870912,
        "time_limit":           60,
        "programming_language": 'bash',
        "natural_language":     'nl',
        "resources":            Path(exercise_dir, 'evaluation'),
        "source":               Path(exercise_dir, 'solution/correct.sh'),
        "judge":                Path('.'),
        "workdir":              Path('workdir'),
        "plan_name":            "one-programmed-correct.tson",
        "timing_statistics":    True,
        "options":              {
            "parallel": False,
            "mode":     "batch",
            "linter":   True
        }
    })


if __name__ == '__main__':
    internal_timings.collect_timings(True)

    config = read_config()

    # Enable logging
    log = logging.getLogger()
    log.setLevel(logging.DEBUG)
    ch = logging.StreamHandler(stream=sys.stdout)
    formatter = logging.Formatter('%(name)s:%(levelname)s:%(message)s')
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
    # run(configs, open(os.devnull, "w"))
    # f = open(f"tests/isbn/students/student{STUDENT}/{EXERCISE}.dson", 'w')
    run(config, sys.stdout)
    end = time.time()
    print()
    print(f"Judging took {end - start} seconds (real time)")
