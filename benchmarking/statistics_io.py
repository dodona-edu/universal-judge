import os
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional

from benchmarking import BenchmarkExercise, BenchmarkResult, get_config, debug, \
    extension_name, BenchmarkResults
from tested.main import run

exercise_dir = Path(__file__).parent.parent / 'exercise'
test_dir = Path(__file__).parent / 'tests'

exercises_io = [
    (exercise_dir / 'echo', "full.tson", "correct"),
    (test_dir / 'Hoe slim ben jij?', "plan.tson", "solution")
]


@dataclass
class BenchmarkExerciseIO(BenchmarkExercise):
    optimized: bool = False
    root_dir: Optional[Path] = None


def time_io_exercises(exercises: List[BenchmarkExerciseIO],
                      times: int = 10) -> BenchmarkResults:
    print("Start warmup")
    time_io_exercise(exercises[0], 1)
    print("Finished warmup")
    len_exercise = len(exercises)
    exercise_results = []
    for index, exercise in enumerate(exercises, start=1):
        print(f"Start exercise {index}/{len_exercise}")
        exercise_results.append(time_io_exercise(exercise, times))
        print(f"Finished exercise {index}/{len_exercise}")
    return BenchmarkResults(
        results=exercise_results
    )


def time_io_exercise(exercise: BenchmarkExerciseIO,
                     times: int = 25,
                     optimized: bool = True) -> BenchmarkResult:
    def timing() -> float:
        config = get_config(exercise, exercise_root=exercise.root_dir,
                            optimized=optimized)
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


def get_all_io_benchmark_exercises():
    exercises = []
    for e, p, f in sorted(exercises_io, key=lambda x: x[0]):
        evaluation_dir = e / 'evaluation'
        solution_dir = e / 'solution'
        if not evaluation_dir.exists() or not solution_dir.exists() or \
                not evaluation_dir.is_dir() or not solution_dir.is_dir():
            continue
        evaluations = [evaluation_dir / p]
        solutions = list(
            filter(lambda x: x.suffix in extension_name and x.name.startswith(f),
                   solution_dir.iterdir()))
        for solution in solutions:
            for evaluation in evaluations:
                exercises.append(BenchmarkExerciseIO(
                    name=e.name,
                    plan=evaluation.name,
                    solution=solution.name,
                    language=extension_name[solution.suffix],
                    optimized=False,
                    root_dir=e.parent
                ))
                if debug:
                    print(exercises[-1])
                exercises.append(BenchmarkExerciseIO(
                    name=e.name,
                    plan=evaluation.name,
                    solution=solution.name,
                    language=extension_name[solution.suffix],
                    optimized=True,
                    root_dir=e.parent
                ))
                if debug:
                    print(exercises[-1])
    return exercises
