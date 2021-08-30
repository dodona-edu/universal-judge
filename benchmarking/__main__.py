import json
import os
import shutil

from pydantic.json import pydantic_encoder

from benchmarking import tmp_dir
from benchmarking.statistics_io import get_all_io_benchmark_exercises, \
    time_io_exercises

# Delete content in work dir
# noinspection PyTypeChecker
for root, dirs, files in os.walk(tmp_dir):
    for f in files:
        os.unlink(os.path.join(root, f))
    for d in dirs:
        shutil.rmtree(os.path.join(root, d), ignore_errors=True)

results = time_io_exercises(get_all_io_benchmark_exercises(), times=10)

with open("benchmarks_io.json", "w") as json_fp:
    json.dump(results, fp=json_fp, default=pydantic_encoder, indent=2)
