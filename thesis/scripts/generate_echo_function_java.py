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

prefix = Path("./src/sources/echo-function")
judge = Path("../judge/src")

prefix.mkdir(parents=True, exist_ok=True)
workdir = Path("./workdir")
shutil.rmtree(workdir, ignore_errors=True)
workdir.mkdir()

args = {
    "memory_limit":         536870912,
    "time_limit":           60,
    "programming_language": 'java',
    "natural_language":     'nl',
    "resources":            '../../exercise/echo-function/evaluation',
    "source":               '../../exercise/echo-function/solution/correct.java',
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


def clean(file):
    with open(file, 'r') as f:
        contents = f.read()
    contents = re.sub(r"(\s)+\n", os.linesep, contents)
    contents = re.sub(r"\{\s+}", "{}", contents)
    contents = contents.strip()
    with open(file, 'w') as f:
        f.write(contents)


# Copy the relevant files.
shutil.copy2(workdir / "common/Context00.java", prefix)
shutil.copy2(workdir / "common/Selector.java", prefix)
shutil.copy2("../exercise/echo/solution/correct.java", prefix)
shutil.rmtree(workdir, ignore_errors=True)

clean(prefix / "Context00.java")
clean(prefix / "Selector.java")
