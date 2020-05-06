"""
Copy code to a directory but warn if the file has changed.
"""
import argparse
import shutil
from pathlib import Path
from typing import Optional

p_destination = Path("./src/sources")


def do_copy(src: Path, destination: Path):
    print(f"Copying {src} to {destination}")
    # noinspection PyTypeChecker
    shutil.copyfile(src, destination)


def copy(src: Path, name: Optional[str]):
    name = name or src.name
    destination = p_destination / name

    if not destination.exists():
        print(f"No existing file for {name}")
        do_copy(src, destination)
        exit(0)

    with open(destination, 'r') as file:
        existing = file.read()

    # noinspection PyTypeChecker
    with open(src, 'r') as file:
        new = file.read()

    existing_lines = len(existing.splitlines())
    new_lines = len(new.splitlines())

    if existing_lines != new_lines:
        print(f"WARNING! {name} has different amount of lines.")
        print("Make sure to search the text and update line numbers if needed.")
        print("Sure to proceed? [yN]")
        answer = input()
        if answer in ["yes", "Y", "y", "true"]:
            do_copy(src, destination)
        exit(0)

    if existing != new:
        print(f"WARNING: contents of {name} are different.")
        do_copy(src, destination)
        exit(0)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Copy files')
    parser.add_argument('file', type=str, help='file to copy')
    parser.add_argument('name', type=str, help='destination name', default=None)
    args = parser.parse_args()
    copy(Path(args.file), name=args.name)
