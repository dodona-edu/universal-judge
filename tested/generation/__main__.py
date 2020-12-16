"""
A command line tool to generate stubs for a language. This tool will ask some
questions and then generate stubs depending on the answer.
"""
import json
import os
import shutil
from pathlib import Path
from mako.template import Template

from tested.utils import pascalize

dir_path = Path(os.path.dirname(os.path.realpath(__file__)))

print("This setup tool will generate various stubs in the correct location for ")
print("adding a new programming language to TESTed. First we require some info.")

name = input("What is the name of the language?\n")
name = name.lower()

is_compiled = input("Does this language need a compilation step? [Yn]  ")

selector = is_compiled in ["Y", "yes", "true", "y", ""]

extension = input(
    "What extension does this language use (without dot, e.g. py, java or c)?"
)

print("Creating directories...")

templates = dir_path.parent / "languages" / name / "templates"

templates.mkdir(parents=True)

print("Generating config.json...")

with open(dir_path / 'config.json', 'r') as f:
    contents = json.load(f)

contents['extensions']['file'] = extension
contents['general']['selector'] = selector

with open(templates.parent / "config.json", "w") as f:
    json.dump(contents, f, indent=2)

print("Generating config class...")
config_class = Template(filename=str(dir_path / "config.mako"))
class_name = pascalize(name)

with open(templates.parent / "config.py", "w") as f:
    f.write(config_class.render(name=class_name, compiled=selector))

print("Generating template stubs...")

shutil.copy(dir_path / "context.mako", templates / f"context.{extension}")
shutil.copy(dir_path / "execution.mako", templates / f"execution.{extension}")
shutil.copy2(dir_path / "selector.mako", templates)
shutil.copy2(dir_path / "value.mako", templates)

if selector:
    shutil.copy(dir_path / "selector.mako", templates / f"selector.{extension}")

print("Done.")
print(f"Find the new files in {templates.parent}.")
print(f"You still need to implement the files; we've only generated stubs.")
