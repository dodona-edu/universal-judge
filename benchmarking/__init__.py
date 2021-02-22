import os
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List

from tested.configs import DodonaConfig
from tested.main import run

tmp_dir = Path("generated")


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


all_exercises = [
    BenchmarkExercise(
        name="combo_breaker",
        plan="plan_opt.json",
        solution="solution.py"
    ),
    BenchmarkExercise(
        name="combo_breaker",
        plan="plan_opt.json",
        solution="solution.old.py"
    ),
    BenchmarkExercise(
        name="combo_breaker",
        plan="plan_no_opt.json",
        solution="solution.py"
    ),
    BenchmarkExercise(
        name="combo_breaker",
        plan="plan_no_opt.json",
        solution="solution.old.py"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="c",
        plan="plan_opt.json",
        solution="solution.c"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="c",
        plan="plan_no_opt.json",
        solution="solution.c"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="python",
        plan="plan_opt.json",
        solution="solution.py"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="python",
        plan="plan_no_opt.json",
        solution="solution.py"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="haskell",
        plan="plan_opt.json",
        solution="solution.hs"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="haskell",
        plan="plan_no_opt.json",
        solution="solution.hs"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="java",
        plan="plan_opt.json",
        solution="solution.java"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="java",
        plan="plan_no_opt.json",
        solution="solution.java"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="kotlin",
        plan="plan_opt.json",
        solution="solution.kt"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="kotlin",
        plan="plan_no_opt.json",
        solution="solution.kt"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="javascript",
        plan="plan_opt.json",
        solution="solution.js"
    ),
    BenchmarkExercise(
        name="password_policy",
        language="javascript",
        plan="plan_no_opt.json",
        solution="solution.js"
    )
]


def get_config(exercise: BenchmarkExercise,
               index: int,
               parallel: bool = True, ) -> DodonaConfig:
    exercise_dir = Path(__file__).parent / 'tests' / exercise.name
    exercise_tmp_dir = get_temp_dir(
        f"{exercise.name}_{exercise.language}_"
        f"{exercise.solution.replace('.', '_')}_"
        f"{exercise.plan.replace('.', '_')}_{index}"
    )
    exercise_tmp_dir.mkdir(parents=True)
    return DodonaConfig(**{
        "memory_limit":         536870912,  # 500 MB
        "time_limit":           120,  # Two minutes
        "programming_language": exercise.language,
        "natural_language":     'nl',
        "resources":            exercise_dir / 'evaluation',
        "source":               exercise_dir / 'solution' / exercise.solution,
        "judge":                Path('.'),
        "workdir":              exercise_tmp_dir,
        "plan_name":            exercise.plan,
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
        config = get_config(exercise, i)
        start = time.perf_counter()
        run(config, open(os.devnull, "w"))
        # run(config, sys.stdout)
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
    len_exercise = len(exercises)
    exercise_results = []
    for index, exercise in enumerate(exercises, start=1):
        print(f"Start exercise {index}/{len_exercise}")
        exercise_results.append(time_exercise(exercise, times))
        print(f"Finished exercise {index}/{len_exercise}")
    return BenchmarkResults(
        results=exercise_results
    )
