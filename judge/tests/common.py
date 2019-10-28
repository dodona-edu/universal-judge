"""Common utilities to test the judge output."""
import json
import os
import re
import subprocess
import tempfile
import jsonschema
import shutil


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
            "source": exercise,
            "judge": JUDGE_DIR,
            "workdir": work_directory,
        }

        shutil.copy2(plan, resource_folder)

        stdin_ = json.dumps(config_)

        p = subprocess.run(['python', f"{JUDGE_DIR}/judge/tested.py"],
                           input=stdin_, text=True, capture_output=True)

        stdout_ = p.stdout
        print(stdout_)
        # Split into jsons
        results = re.compile('(?<=})\\s*(?={)').split(stdout_)
        schema = json.load(schema_file)
        for result in results:
            result = json.loads(result)
            jsonschema.validate(result, schema)


if __name__ == '__main__':
    validate_output(f"{JUDGE_DIR}/exercise/test.py", f"{JUDGE_DIR}/exercise/basic.json")
