import os
import shutil
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List

from tested.configs import DodonaConfig
from tested.dsl import SchemaParser
from tested.main import run

tmp_dir = Path("generated")
debug = False

extension_name = {
    ".c":    "c",
    ".hs":   "haskell",
    ".java": "java",
    ".js":   "javascript",
    ".kt":   "kotlin",
    ".py":   "python"
}


@dataclass
class BenchmarkExercise:
    name: str
    plan: str
    solution: str
    language: str = "python"


@dataclass
class BenchmarkResult:
    exercise: BenchmarkExercise
    min_time: float
    avg_time: float
    max_time: float


@dataclass
class BenchmarkResults:
    results: List[BenchmarkResult]


def get_all_benchmark_exercises() -> List[BenchmarkExercise]:
    benchmark_dir = Path(__file__).parent / 'tests'
    exercises = []
    for p in sorted(benchmark_dir.iterdir()):
        evaluation_dir = p / 'evaluation'
        solution_dir = p / 'solution'
        if not evaluation_dir.exists() or not solution_dir.exists() or \
                not evaluation_dir.is_dir() or not solution_dir.is_dir():
            continue
        evaluations = list(filter(lambda x: x.suffix == ".json",
                                  evaluation_dir.iterdir()))
        solutions = list(filter(lambda x: x.suffix in extension_name,
                                solution_dir.iterdir()))
        for solution in solutions:
            for evaluation in evaluations:
                exercises.append(BenchmarkExercise(
                    name=p.name,
                    plan=evaluation.name,
                    solution=solution.name,
                    language=extension_name[solution.suffix]
                ))
                if debug:
                    print(exercises[-1])
    return exercises


def get_config(exercise: BenchmarkExercise,
               parallel: bool = True, ) -> DodonaConfig:
    exercise_dir = Path(__file__).parent / 'tests' / exercise.name
    workdir = exercise_dir / 'workdir'
    exercise_tmp_dir = get_temp_dir(
        f"{exercise.name}_{exercise.language}_"
        f"{exercise.solution.replace('.', '_').replace(' ', '_')}_"
        f"{exercise.plan.replace('.', '_').replace(' ', '_')}"
    )
    if exercise_tmp_dir.exists():
        shutil.rmtree(exercise_tmp_dir, ignore_errors=True)
    exercise_tmp_dir.mkdir(parents=True)
    if workdir.exists() and workdir.is_dir():
        shutil.copytree(workdir, exercise_tmp_dir, dirs_exist_ok=True)
    return DodonaConfig(**{
        "memory_limit":         536870912,  # 500 MB
        "time_limit":           120,  # Two minutes
        "programming_language": exercise.language,
        "natural_language":     'nl',
        "resources":            exercise_dir / 'evaluation',
        "source":               exercise_dir / 'solution' / exercise.solution,
        "judge":                Path('.'),
        "workdir":              exercise_tmp_dir,
        "testplan":            exercise.plan,
        "options":              {
            "parallel": parallel,
            "mode":     "batch",
            "linter":   {
                "python": False
            }
        }
    })


def get_temp_dir(exercise_name: str) -> Path:
    return Path(tmp_dir, exercise_name)


def time_exercise(exercise: BenchmarkExercise,
                  times: int = 10) -> BenchmarkResult:
    def timing() -> float:
        config = get_config(exercise)
        start = time.perf_counter()
        if not debug:
            run(config, open(os.devnull, "w"))
        else:
            run(config, sys.stdout)
        end = time.perf_counter()
        return end - start

    timing_results = []

    for i in range(1, times + 1):
        print(f"Time {i}/{times}")
        timing_results.append(timing())

    return BenchmarkResult(
        exercise=exercise,
        min_time=min(timing_results),
        avg_time=sum(timing_results) / len(timing_results),
        max_time=max(timing_results)
    )


def time_exercises(exercises: List[BenchmarkExercise],
                   times: int = 10) -> BenchmarkResults:
    print("Start warmup")
    time_exercise(exercises[0], 1)
    print("Finished warmup")
    len_exercise = len(exercises)
    exercise_results = []
    for index, exercise in enumerate(exercises, start=1):
        print(f"Start exercise {index}/{len_exercise}")
        exercise_results.append(time_exercise(exercise, times))
        print(f"Finished exercise {index}/{len_exercise}")
    return BenchmarkResults(
        results=exercise_results
    )
