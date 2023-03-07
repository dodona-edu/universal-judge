"""
Module for the timings.
"""
import os
import sys
import tempfile

import time
from pathlib import Path

import tested
from tested.configs import DodonaConfig, Options
from tested.main import run
from tested.testsuite import ExecutionMode

amount = 3


def timing(config: DodonaConfig) -> float:
    start = time.perf_counter()
    tested.main.run(config, open(os.devnull, "w"))
    # tested.main.run(config, sys.stdout)
    end = time.perf_counter()
    return end - start


def timings(config: DodonaConfig, nr: int = 3) -> float:
    results = []
    for _ in range(nr):
        with tempfile.TemporaryDirectory() as working_dir:
            config.workdir = Path(working_dir)
            # config.workdir = Path("../judge/workdir")
            results.append(timing(config))
    assert len(results) == nr
    return sum(results) / nr


def python_lotto_config(options: Options) -> DodonaConfig:
    return DodonaConfig(
        resources=Path("../exercise/lotto/evaluation"),
        source=Path("../exercise/lotto/solution/correct.py"),
        time_limit="600",
        memory_limit="50000",
        natural_language="nl",
        programming_language="python",
        workdir=Path("."),
        judge=Path("../judge/src/"),
        options=options,
    )


def lotto_python_context_single():
    return timings(
        python_lotto_config(
            Options(parallel=False, mode=ExecutionMode.INDIVIDUAL, optimized=False)
        )
    )


def lotto_python_context_threaded():
    return timings(
        python_lotto_config(
            Options(parallel=True, mode=ExecutionMode.INDIVIDUAL, optimized=False)
        )
    )


def lotto_python_batch_single():
    return timings(
        python_lotto_config(
            Options(parallel=False, mode=ExecutionMode.PRECOMPILATION, optimized=False)
        )
    )


def lotto_python_batch_threaded():
    return timings(
        python_lotto_config(
            Options(parallel=True, mode=ExecutionMode.PRECOMPILATION, optimized=False)
        )
    )


def lotto_python_exec_single():
    return timings(
        python_lotto_config(
            Options(parallel=False, mode=ExecutionMode.PRECOMPILATION, optimized=True)
        )
    )


def lotto_python_exec_threaded():
    return timings(
        python_lotto_config(
            Options(parallel=True, mode=ExecutionMode.PRECOMPILATION, optimized=True)
        )
    )


def lotto_python():
    lpcs = lotto_python_context_single()
    lpct = lotto_python_context_threaded()
    lpbs = lotto_python_batch_single()
    lpbt = lotto_python_batch_threaded()
    lpes = lotto_python_exec_single()
    lpet = lotto_python_exec_threaded()

    # lpcs = 1
    # lpct = 1
    # lpbs = 1
    # lpbt = 1
    # lpes = 1
    # lpet = 1

    print(
        f"""
                  single\tparallel
lotto   context   {lpcs}\t\t\t{lpct}
        batch     {lpbs}\t\t\t{lpbt}
        exec      {lpes}\t\t\t{lpet}  
"""
    )


def java_lotto_config(options: Options) -> DodonaConfig:
    return DodonaConfig(
        resources=Path("../exercise/lotto/evaluation"),
        source=Path("../exercise/lotto/solution/correct.java"),
        time_limit="600",
        memory_limit="50000",
        natural_language="nl",
        programming_language="java",
        workdir=Path("."),
        judge=Path("../judge/src/"),
        options=options,
    )


def lotto_java_context_single():
    return timings(
        java_lotto_config(
            Options(parallel=False, mode=ExecutionMode.INDIVIDUAL, optimized=False)
        )
    )


def lotto_java_context_threaded():
    return timings(
        java_lotto_config(
            Options(parallel=True, mode=ExecutionMode.INDIVIDUAL, optimized=False)
        )
    )


def lotto_java_batch_single():
    return timings(
        java_lotto_config(
            Options(parallel=False, mode=ExecutionMode.PRECOMPILATION, optimized=False)
        )
    )


def lotto_java_batch_threaded():
    return timings(
        java_lotto_config(
            Options(parallel=True, mode=ExecutionMode.PRECOMPILATION, optimized=False)
        )
    )


def lotto_java_exec_single():
    return timings(
        java_lotto_config(
            Options(parallel=False, mode=ExecutionMode.PRECOMPILATION, optimized=True)
        )
    )


def lotto_java_exec_threaded():
    return timings(
        java_lotto_config(
            Options(parallel=True, mode=ExecutionMode.PRECOMPILATION, optimized=True)
        )
    )


def lotto_java():
    # ljcs = lotto_java_context_single()
    print("1/6")
    ljct = lotto_java_context_threaded()
    print("2/6")
    ljbs = lotto_java_batch_single()
    print("3/6")
    ljbt = lotto_java_batch_threaded()
    print("4/6")
    ljes = lotto_java_exec_single()
    print("5/6")
    ljet = lotto_java_exec_threaded()

    ljcs = 51.52
    # ljct = 1
    # ljbs = 1
    # ljbt = 1
    # ljes = 1
    # ljet = 1

    print(
        f"""
                  single\tparallel
lotto   context   {ljcs}\t\t\t{ljct}
        batch     {ljbs}\t\t\t{ljbt}
        exec      {ljes}\t\t\t{ljet}  
"""
    )


if __name__ == "__main__":
    lotto_java()
