"""Common utilities to test the judge output."""
import json
import os
import re
import subprocess
import tempfile
import jsonschema
import shutil

from jsonschema import ValidationError

TEST_DIR = os.path.dirname(__file__)
JUDGE_DIR = os.path.dirname(os.path.dirname(TEST_DIR))


def validate_output(exercise, plan):
    """
    Validates that the output for a particular exercises is valid output according to the JSON
    schema of the judge output.
    :param plan: the testplan for this exercise
    :param exercise: path to the exercise file
    """
    with tempfile.TemporaryDirectory() as resource_folder, \
            tempfile.TemporaryDirectory() as work_directory, \
            open(f"{TEST_DIR}/partial_output.json", "r") as schema_file:
        config_ = {
            "memory_limit": 536870912,
            "time_limit": 1000,
            "programming_language": 'python',
            "natural_language": 'nl',
            "resources": resource_folder,
            "source": str(exercise),
            "judge": JUDGE_DIR,
            "workdir": work_directory,
        }

        shutil.copy2(plan, resource_folder + "/full.json")

        stdin_ = json.dumps(config_)

        p = subprocess.run(['python', f"{JUDGE_DIR}/judge/tested.py"],
                           input=stdin_, text=True, capture_output=True)

        stdout_ = p.stdout
        # Split into jsons
        results = re.compile('(?<=})\\s*(?={)').split(stdout_)
        schema = json.load(schema_file)
        errors = []
        for result in results:
            try:
                if result:
                    result = json.loads(result)
                    jsonschema.validate(result, schema)
            except ValidationError as e:
                errors.append(e)
        for error in errors:
            raise error
        assert errors == []


def validate_result(exercise, plan, expected):
    """
    Validate that the judge output corresponds to the expected output.
    :param exercise: path to the exercise file
    :param plan: the testplan for this exercise
    :param expected: path to the file containing the expected judge output
    """
    with tempfile.TemporaryDirectory() as resource_folder, \
            tempfile.TemporaryDirectory() as work_directory, \
            open(f"{expected}", "r") as expected_output:
        config_ = {
            "memory_limit": 536870912,
            "time_limit": 1000,
            "programming_language": 'python',
            "natural_language": 'nl',
            "resources": resource_folder,
            "source": str(exercise),
            "judge": JUDGE_DIR,
            "workdir": work_directory,
        }

        shutil.copy2(plan, resource_folder + "/full.json")
        stdin_ = json.dumps(config_)
        p = subprocess.run(['python', f"{JUDGE_DIR}/judge/tested.py"],
                           input=stdin_, text=True, capture_output=True)
        actual_output = p.stdout.strip()
        expected_output = "".join(expected_output.read().splitlines())

        assert actual_output == expected_output
