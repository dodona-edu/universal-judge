"""
Run the judge manually from code. In this mode, the config is hardcoded into this file, allowing
rapid testing (and, most importantly, debugging).
"""
import os
import shutil
import time
import sys
from pathlib import Path

from tested import Config, run
from testplan import parse_test_plan


def read_config() -> Config:
    """Read the configuration from stdout"""
    return Config(**{
        "memory_limit": 536870912,
        "time_limit": 10000000,
        "programming_language": 'python',
        "natural_language": 'nl',
        "resources": str(Path('../exercise/lotto/evaluation').resolve()),
        "source": '../exercise/lotto/solution/correct.py',
        "judge": str(Path('../').resolve()),
        "workdir": str(Path('./workdir').resolve()),
        "plan_name": "plan.json"
    })


if __name__ == '__main__':
    config = read_config()

    # Delete content in work dir
    for root, dirs, files in os.walk(config.workdir):
        for f in files:
            os.unlink(os.path.join(root, f))
        for d in dirs:
            shutil.rmtree(os.path.join(root, d))

    start = time.time()
    run(config, sys.stdout)
    end = time.time()
    print()
    print(f"Judging took {end - start} seconds (real time)")
