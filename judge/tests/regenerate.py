"""
Generate all expected values for the tests.
Only do this if you are sure the results are correct!
"""
import os

from tested import Config, run, clean_working_directory
from pathlib import Path


# noinspection PyShadowingNames
def all_submissions():
    iter_ = os.walk("tests/isbn/students/")
    next(iter_)
    data = []
    for root, subdirs, files in iter_:
        student = Path(root).stem
        submissions = {Path(x).stem for x in files}
        data.append((student, submissions))

    final_data = []
    for student, submissions in data:
        for submission in submissions:
            final_data.append((student, submission))
    return final_data


# noinspection PyShadowingNames
def read_config(student: str, exercise: str, workdir: str) -> Config:
    """Read the configuration from stdout"""
    return Config(**{
        "memory_limit": 536870912,
        "time_limit": 10000000,
        "programming_language": 'python',
        "natural_language": 'nl',
        "resources": str(Path('../exercise/isbn/evaluation').resolve()),
        "source": f'tests/isbn/students/{student}/{exercise}.py',
        "judge": str(Path('../').resolve()),
        "workdir": workdir,
        "plan_name": "plan.json",
    })


if __name__ == '__main__':
    print("WARNING! Only execute this if you are sure results are correct!")
    final_data = all_submissions()
    total = len(final_data)
    for index, (student, exercise) in enumerate(final_data):
        print(f"Updating expected for {student}, {exercise} [{index+1}/{total}]")
        config = read_config(student, exercise, str(Path('./workdir').resolve()))
        clean_working_directory(config)
        with open(f"tests/isbn/students/{student}/{exercise}.dson", 'w') as output_file:
            run(config, output_file)
