"""
Test the judge using a Python exercise.
The original exercise is located at
https://github.ugent.be/pythia/programmeren/tree/master/opgaven/reeks07/ISBN
"""
import os
import shutil
import tempfile
from io import StringIO

from tested import run
from tests.regenerate import read_config


def test_submission(student: str, submission: str):

    with tempfile.TemporaryDirectory() as tmpdirname:
        config = read_config(student, submission, tmpdirname)

        # Delete content in work dir
        for root, dirs, files in os.walk(config.workdir):
            for f in files:
                os.unlink(os.path.join(root, f))
            for d in dirs:
                shutil.rmtree(os.path.join(root, d), ignore_errors=True)
        actual = StringIO()
        run(config, actual)

        with open(f'tests/isbn/students/{student}/{submission}.dson', 'r') as f:
            expected_string = f.read()
        actual_string = actual.getvalue()

        assert expected_string == actual_string
