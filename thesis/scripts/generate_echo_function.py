"""
Generate the data for the appendix with the ECHO exercise.
"""
import json
import shutil
import subprocess
from pathlib import Path

import sys

prefix = Path("./src/sources/echo-function-c")
judge = Path("../judge/src")

prefix.mkdir(parents=True, exist_ok=True)
workdir = Path("./workdir")
shutil.rmtree(workdir, ignore_errors=True)
workdir.mkdir()

args = {
    "memory_limit":         536870912,
    "time_limit":           60,
    "programming_language": 'c',
    "natural_language":     'nl',
    "resources":            '../../exercise/echo-function/evaluation',
    "source":               '../../exercise/echo-function/solution/correct.c',
    "judge":                str(Path('..').resolve()),
    "workdir":              str(workdir.resolve()),
    "plan_name":            "one-testcase.tson",
    "options":              {
        "parallel": True,
        "linter": {
            "python": False
        }
    }
}

stdin = json.dumps(args)
r = subprocess.run(["python", "-m", "tested"], cwd=judge, input=stdin, text=True, capture_output=True)
sys.stderr.write(r.stderr)

# Copy the relevant files.
shutil.copy2(workdir / "common/context_0_0.c", prefix)
shutil.copy2(workdir / "common/selector.c", prefix)
shutil.copy2("../exercise/echo-function/description/description.nl.md", prefix / "description.md")
shutil.copy2("../exercise/echo-function/evaluation/one-testcase.tson", prefix)
shutil.copy2("../exercise/echo-function/solution/correct.c", prefix)
shutil.rmtree(workdir, ignore_errors=True)
