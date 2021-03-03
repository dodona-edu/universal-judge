from collections import defaultdict
from typing import Tuple, Dict, List

from pydantic import BaseModel

from benchmarking import BenchmarkResults, BenchmarkResult


class _BenchmarkResultsModel(BaseModel):
    __root__: BenchmarkResults


def compute_relative_average_speedups_from_language(
        exercises: Dict[str, List[BenchmarkResult]]
) -> Dict[str, Tuple[str, float, float, float]]:
    speedups = dict()
    for key, value in exercises.items():
        solutions = get_solution_names(value)
        for solution in solutions:
            opt, no_opt = select_optimized_and_not_optimized(value, solution)
            _, avg_speedup, _ = compute_relative_speedup(no_opt, opt)
            speedups[key] = (solution, avg_speedup, no_opt.avg_time, opt.avg_time)
    return speedups


def compute_relative_speedup(result_no_opt: BenchmarkResult,
                             result_opt: BenchmarkResult
                             ) -> Tuple[float, float, float]:
    return (
        result_no_opt.min_time / result_opt.min_time,
        result_no_opt.avg_time / result_opt.avg_time,
        result_no_opt.max_time / result_opt.max_time
    )


def get_solution_names(benchmark_results: List[BenchmarkResult]):
    return [
        name for name in
        sorted(set(map(lambda x: x.exercise.solution, benchmark_results)))
    ]


def group_result_by_language_and_exercise(
        benchmark_results: BenchmarkResults
) -> Dict[str, Dict[str, List[BenchmarkResult]]]:
    group_by_languages_exercise = defaultdict(lambda: defaultdict(list))
    for result in benchmark_results.results:
        exercise = result.exercise
        group_by_languages_exercise[exercise.language][exercise.name].append(result)
    return group_by_languages_exercise


def read_benchmarks(file: str = "benchmarks.json") -> BenchmarkResults:
    with open(file, 'r') as fd:
        json_string = fd.read()
    return _BenchmarkResultsModel.parse_raw(json_string).__root__


def select_optimized_and_not_optimized(benchmark_results: List[BenchmarkResult],
                                       solution_name: str
                                       ) -> Tuple[BenchmarkResult, BenchmarkResult]:
    filtered = filter(lambda x: x.exercise.solution == solution_name,
                      benchmark_results)
    not_optimized = next(
        iter(filter(lambda x: 'no_opt' in x.exercise.plan, filtered)))
    optimized = next(
        iter(filter(lambda x: 'no_opt' not in x.exercise.plan, filtered)))
    return optimized, not_optimized


def write_csv_language(language: str,
                       statistics: Dict[str, Tuple[str, float, float, float]]):
    with open(f"statistics_{language}.csv", 'w') as csv_file:
        print('"Exercise","Solution","Speedup","Absolute time not optimized",'
              '"Absolute time optimized"', file=csv_file)
        for exercise, (
                solution, speedup, time_not_optimized, time_optimized
        ) in statistics.items():
            print(f'"{exercise}","{solution}",{speedup},{time_not_optimized},'
                  f'{time_optimized}', file=csv_file)
        csv_file.close()


if __name__ == "__main__":
    benchmarks = read_benchmarks()
    grouped = group_result_by_language_and_exercise(benchmarks)
    for lang, results in sorted(grouped.items(), key=lambda x: x[0]):
        r = compute_relative_average_speedups_from_language(results)
        write_csv_language(lang, r)
