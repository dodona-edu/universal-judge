"""
Extract some docstrings as markdown.
"""

import argparse
import re
import subprocess
import sys

parser = argparse.ArgumentParser(description='Process Markdown for Mathjax.')
parser.add_argument('output', type=str, help='filename for output')
parser.add_argument('input', type=str, help='what to use as input', nargs="+")

args = parser.parse_args()
result = subprocess.run(["python", "-m", "pydocmd", "simple", *args.input],
                        cwd="../judge", capture_output=True, check=True, text=True)

filtered = re.sub(r"(```.+?```)", '', result.stdout, flags=re.MULTILINE | re.DOTALL)

with open(args.output, "w") as f_out:
    f_out.write(filtered)
