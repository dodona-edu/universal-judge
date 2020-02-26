"""
Test the judge using a Python exercise.
The original exercise is located at
https://github.ugent.be/pythia/programmeren/tree/master/opgaven/reeks07/ISBN
"""
import os
import shutil
import tempfile
from io import StringIO
from tested import Config, run
from pathlib import Path


def test_submission(student: str, submission: str):

    with tempfile.TemporaryDirectory() as tmpdirname:
        config = Config(**{
            "memory_limit": 536870912,
            "time_limit": 10000000,
            "programming_language": 'python',
            "natural_language": 'nl',
            "resources": str(Path('../../exercise/isbn/evaluation').resolve()),
            "source": f'isbn/students/{student}/{submission}.py',
            "judge": str(Path('../../').resolve()),
            "workdir": tmpdirname,
            "plan_name": "plan.json"
        })
        # Delete content in work dir
        for root, dirs, files in os.walk(config.workdir):
            for f in files:
                os.unlink(os.path.join(root, f))
            for d in dirs:
                shutil.rmtree(os.path.join(root, d), ignore_errors=True)
        actual = StringIO()
        run(config, actual)

        with open(f'isbn/students/{student}/{submission}.dson', 'r') as f:
            expected_string = f.read()
        actual_string = actual.getvalue()

        assert expected_string == actual_string


if __name__ == '__main__':
    test_submission("student01", "3327003")
