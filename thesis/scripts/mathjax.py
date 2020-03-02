"""
Script that will remove mathjax stuff from markdown files.
"""
import argparse
import os

parser = argparse.ArgumentParser(description='Process Markdown for Mathjax.')
parser.add_argument('input', type=str, help='filename for input')
parser.add_argument('output', type=str, help='filename for output')

args = parser.parse_args()

with open(args.input, "r") as f_in:
    contents = f_in.read()

filtered = contents.replace("$$", "$")

os.makedirs(os.path.dirname(args.output), exist_ok=True)

with open(args.output, "w") as f_out:
    f_out.write(filtered)
