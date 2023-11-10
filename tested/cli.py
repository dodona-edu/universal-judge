"""
A module that provides a simpler CLI to TESTed.
"""
import json
import os
import re
import shutil
import textwrap
import time
from argparse import ArgumentParser, ArgumentTypeError
from io import StringIO
from pathlib import Path

from tested.configs import DodonaConfig, GlobalConfig
from tested.languages import get_language
from tested.main import run
from tested.testsuite import Suite, SupportedLanguage


def dir_path(path):
    if os.path.isdir(path):
        return Path(path)
    else:
        raise ArgumentTypeError(f"{path} is not a valid path (cwd: {os.getcwd()})")


def find_submission() -> Path:
    # First, if we have an explicit submission, use that.
    if args.submission is not None:
        return Path(args.submission)
    # Attempt to determine the programming language of this exercise.
    considered = []
    # There is a bit of a chicken-and-egg problem here.
    # We need the language to complete the config, but the language needs a config.
    # We thus create a dummy config.
    global_config = GlobalConfig(
        dodona=DodonaConfig(
            resources=Path("."),
            source=Path("."),
            time_limit=10,
            memory_limit=10,
            natural_language="none",
            programming_language=SupportedLanguage(programming_language),
            workdir=Path("."),
            judge=Path("."),
        ),
        testcase_separator_secret="",
        context_separator_secret="",
        suite=Suite(),
    )
    language = get_language(global_config, programming_language)
    solution_folder = exercise_path / "solution"
    possible_submissions = list(solution_folder.glob(f"*.{language.file_extension()}"))

    if len(possible_submissions) == 0:
        raise FileNotFoundError(
            f"Could not find a submission file in {solution_folder}/*.{language.file_extension()}.\n"
            "Please ensure a submission file exists in the right location or provide an alternative path via the --submission parameter on the command line."
        )

    return possible_submissions[0]


def create_and_populate_workdir(config: DodonaConfig):
    # Create workdir if needed.
    config.workdir.mkdir(exist_ok=True)

    # Delete content in work dir
    for root, dirs, files in os.walk(config.workdir):
        for f in files:
            os.unlink(os.path.join(root, f))
        for d in dirs:
            shutil.rmtree(os.path.join(root, d), ignore_errors=True)

    exercise_workdir = config.resources.parent / "workdir"

    # Copy existing files to workdir if needed.
    if exercise_workdir.is_dir():
        shutil.copytree(exercise_workdir, config.workdir, dirs_exist_ok=True)


def split_output(to_split: str) -> list[str]:
    return re.compile(r"(?<=})\s*(?={)").split(to_split)


class CommandDict(list):
    def find_all(self, command: str) -> list[dict]:
        return [x for x in self if x["command"] == command]

    def find_next(self, command: str) -> dict:
        return next(x for x in self if x["command"] == command)

    def find_status_enum(self) -> list[str]:
        commands = [
            x
            for x in self
            if x["command"].startswith("close-") or x["command"] == "escalate-status"
        ]
        return [x["status"]["enum"] for x in commands if "status" in x]


if __name__ == "__main__":
    parser = ArgumentParser(
        description="Simple CLI for TESTed",
        epilog=textwrap.dedent(
            """
                 additional information:\n
                     The CLI only looks at a config.json file in the exercise directory.\n
                     It does not look in folders above the exercise directory.
                 """
        ),
    )
    parser.add_argument(
        "-e",
        "--exercise",
        type=dir_path,
        help="Path to a directory containing an exercise",
        required=True,
    )
    parser.add_argument(
        "-s",
        "--submission",
        type=Path,
        help="Path to a submission to evaluate",
        default=None,
    )
    parser.add_argument(
        "-t",
        "--testsuite",
        type=Path,
        help="Path to a test suite",
        default=None,
    )
    parser.add_argument(
        "-f",
        "--full",
        action="store_false",
        help="If the output should be shown in full (default: false)",
        default=None,
    )
    parser.add_argument(
        "-p",
        "--programming_language",
        help="The programming language to use",
        default=None,
    )
    args = parser.parse_args()

    exercise_path = args.exercise
    evaluation_path = exercise_path / "evaluation"
    judge_path = Path(__file__).parent.parent
    config_path = exercise_path / "config.json"

    try:
        config_file = json.loads(config_path.read_text())
    except FileNotFoundError:
        config_file = dict()

    if args.programming_language is not None:
        programming_language = args.programming_language
    else:
        try:
            programming_language = config_file["programming_language"]
        except KeyError:
            if config_path.exists():
                raise Exception(
                    f"Could not determine the programming language for {exercise_path}, as there was no programming language in the config file (at {config_path}).\n"
                    "Please add a programming language to the config file or provide the programming language via the --programming_language parameter on the command line."
                )
            else:
                raise Exception(
                    f"Could not determine the programming language for {exercise_path}, as there was no config file (at {config_path}) for this exercise.\n"
                    "Please create a config file or provide the programming language via the --programming_language parameter on the command line."
                )

    if args.testsuite is not None:
        suite_path = args.testsuite.relative_to(evaluation_path)
    elif "evaluation" in config_file and "test_suite" in config_file["evaluation"]:
        # The config file contains a location, so try to use it.
        suite_path = evaluation_path / config_file["evaluation"]["test_suite"]
        if not suite_path.is_file():
            raise FileNotFoundError(
                f"The test suite at {suite_path} does not exist (read value from the config file).\n"
                "Create the file, correct the config.json file or provide the path to the test suite via the --testsuite parameter on the command line."
            )
    else:
        suite_path = evaluation_path / "suite.yaml"
        if not suite_path.is_file():
            raise FileNotFoundError(
                f"The test suite at {suite_path} does not exist (used default value).\n"
                "Create the file, add the location to the config.json file or provide the path to the test suite via the --testsuite parameter on the command line."
            )
    submission_path = find_submission()
    workdir_path = judge_path / "workdir"

    dodona_config = DodonaConfig(
        resources=evaluation_path,
        source=submission_path,
        time_limit=60,
        memory_limit=536870912,
        natural_language="nl",
        programming_language=SupportedLanguage(programming_language),
        workdir=judge_path / "workdir",
        judge=judge_path,
        test_suite=suite_path,
    )

    output_handler = StringIO()

    create_and_populate_workdir(dodona_config)

    print(f"Locally executing exercise {exercise_path}...")
    print("The following options will be used:")
    print(f" - Test suite: {suite_path}")
    print(f" - Submission: {submission_path}")
    print("")
    print(
        f"The execution will happen in {workdir_path}. This folder will remain available after execution for inspection."
    )

    start = time.time()
    run(dodona_config, output_handler)
    end = time.time()
    print()
    print(f"Execution took {end - start} seconds (real time).")

    if args.full:
        print("Results:")
        print(output_handler.getvalue())
    else:
        output = output_handler.getvalue()
        updates = CommandDict()
        # Every update should be valid.
        for update in split_output(output):
            update_object = json.loads(update)
            updates.append(update_object)

        results = updates.find_status_enum()
        correct_enums = len([x for x in results if x == "correct"])
        print(f"{correct_enums} of {len(results)} testcases were correct.")
