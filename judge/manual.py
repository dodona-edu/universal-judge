"""
Run the judge manually from code. In this mode, the config is hardcoded into this
file, allowing rapid testing (and, most importantly, debugging).
"""
import logging
import os
import sys

import shutil
import time
from pathlib import Path

from tested import Config, run

STUDENT = "01"
EXERCISE = "3326958"


def read_config() -> Config:
    """Read the configuration from stdout"""
    return Config(**{
        "memory_limit": 536870912,
        "time_limit": 10000000,
        "programming_language": 'python',
        "natural_language": 'nl',
        "resources": str(Path('../exercise/isbn/evaluation').resolve()),
        "source": f'tests/isbn/students/student{STUDENT}/{EXERCISE}.py',
        "judge": str(Path('../').resolve()),
        "workdir": str(Path('./workdir').resolve()),
        "plan_name": "plan.json"
    })


if __name__ == '__main__':
    config = read_config()

    # Enable logging
    log = logging.getLogger()
    log.setLevel(logging.DEBUG)
    ch = logging.StreamHandler(stream=sys.stdout)
    formatter = logging.Formatter('%(name)s:%(levelname)s:%(message)s')
    ch.setFormatter(formatter)
    log.addHandler(ch)

    # Delete content in work dir
    for root, dirs, files in os.walk(config.workdir):
        for f in files:
            os.unlink(os.path.join(root, f))
        for d in dirs:
            shutil.rmtree(os.path.join(root, d), ignore_errors=True)

    start = time.time()
    # run(config, open(os.devnull, "w"))
    # f = open(f"tests/isbn/students/student{STUDENT}/{EXERCISE}.dson", 'w')
    run(config, sys.stdout)
    end = time.time()
    print()
    print(f"Judging took {end - start} seconds (real time)")
