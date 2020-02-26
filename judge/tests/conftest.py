import os

import pytest
from pathlib import Path


def pytest_generate_tests(metafunc):
    if "student" in metafunc.fixturenames and "submission" in metafunc.fixturenames:
        iter = os.walk("isbn/students/")
        next(iter)
        data = []
        for root, subdirs, files in iter:
            student = Path(root).stem
            submissions = {Path(x).stem for x in files}
            data.append((student, submissions))

        final_data = []
        for student, submissions in data:
            for submission in submissions:
                final_data.append((student, submission))

        metafunc.parametrize(["student", "submission"], final_data)
