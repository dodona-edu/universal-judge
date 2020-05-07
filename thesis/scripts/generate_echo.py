"""
Generate the data for the appendix with the ECHO exercise.
"""
import json
import os
import re
import shutil
import subprocess
from pathlib import Path

import sys

prefix = Path("./src/sources/echo-c")
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
    "resources":            '../../exercise/echo/evaluation',
    "source":               '../../exercise/echo/solution/correct.c',
    "judge":                str(Path('..').resolve()),
    "workdir":              str(workdir.resolve()),
    "plan_name":            "two.tson",
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
shutil.copy2(workdir / "common/context_0_1.c", prefix)
shutil.copy2(workdir / "common/selector.c", prefix)
shutil.copy2("../exercise/echo/description/description.nl.md", prefix / "description.md")
shutil.copy2("../exercise/echo/evaluation/two.tson", prefix)
shutil.copy2("../exercise/echo/solution/correct.c", prefix)
shutil.rmtree(workdir, ignore_errors=True)


def clean(file):
    with open(file, 'r') as f:
        contents = f.read()
    contents = re.sub(r"(\s)+\n", os.linesep, contents)
    contents = contents.strip()
    with open(file, 'w') as f:
        f.write(contents)


clean(prefix / "context_0_0.c")
clean(prefix / "context_0_1.c")
clean(prefix / "selector.c")
