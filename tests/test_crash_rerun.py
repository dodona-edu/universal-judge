"""
Tests for running the contexts a crash left unexecuted in a new process.
"""

import time
from pathlib import Path
from unittest.mock import MagicMock

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Status
from tested.judge import core
from tested.judge.core import _rerun_after_crash, _UnitRun
from tested.judge.execution import ExecutionResult
from tested.judge.planning import (
    CompilationResult,
    ExecutionPlan,
    PlannedExecutionUnit,
)

SEPARATOR = "--secret-- SEP"


def _execution(
    contexts: int,
    exit_code: int = -11,
    timeout: bool = False,
    memory: bool = False,
    stream: str = "stdout",
) -> ExecutionResult:
    """The result of a process that printed the separators of the given contexts."""
    streams = {"stdout": "", "stderr": "", "results": "", "exceptions": ""}
    streams[stream] = SEPARATOR * contexts
    return ExecutionResult(
        exit=exit_code,
        timeout=timeout,
        memory=memory,
        context_separator=SEPARATOR,
        testcase_separator="--other-- SEP",
        **streams,
    )


def _run(execution: ExecutionResult | None) -> _UnitRun:
    return _UnitRun(CompilationResult(status=Status.CORRECT), Path("."), execution)


def _plan(contexts: int, max_time: float = 100) -> ExecutionPlan:
    unit = PlannedExecutionUnit(
        contexts=[MagicMock(name=f"context {i}") for i in range(contexts)],
        name="execution_0",
        index=0,
    )
    return ExecutionPlan(
        units=[unit],
        common_directory=Path("."),
        selector=None,
        max_time=max_time,
        start_time=time.perf_counter(),
        files=[],
    )


@pytest.mark.parametrize("stream", ["stdout", "stderr", "results", "exceptions"])
def test_crash_in_a_context_can_resume(stream: str):
    assert _run(_execution(1, stream=stream)).can_resume()


@pytest.mark.parametrize(
    "execution",
    [
        # The process crashed before it reached its first context.
        _execution(0),
        _execution(1, exit_code=0, timeout=True),
        _execution(1, exit_code=-9, memory=True),
        None,
    ],
)
def test_run_that_cannot_resume(execution: ExecutionResult | None):
    assert not _run(execution).can_resume()


def test_run_without_crash_is_not_rerun(mocker: MockerFixture):
    rerun = mocker.patch.object(core, "_execute_contexts")
    plan = _plan(3)
    first = _run(_execution(3, exit_code=0))
    assert _rerun_after_crash(MagicMock(), plan, 0, first) == [first]
    rerun.assert_not_called()


def test_crashes_are_rerun_until_all_contexts_ran(mocker: MockerFixture):
    reruns = [_run(_execution(1)), _run(_execution(2, exit_code=0))]
    rerun = mocker.patch.object(core, "_execute_contexts", side_effect=reruns)
    plan = _plan(4)
    first = _run(_execution(1))
    assert _rerun_after_crash(MagicMock(), plan, 0, first) == [first, *reruns]
    contexts = plan.units[0].contexts
    assert [c.args[2:] for c in rerun.call_args_list] == [
        (contexts[1:], 1),
        (contexts[2:], 2),
    ]


def test_rerun_that_does_not_compile_stops(mocker: MockerFixture):
    rerun = mocker.patch.object(core, "_execute_contexts", return_value=None)
    first = _run(_execution(1))
    assert _rerun_after_crash(MagicMock(), _plan(3), 0, first) == [first]
    rerun.assert_called_once()


def test_no_rerun_without_time_left(mocker: MockerFixture):
    rerun = mocker.patch.object(core, "_execute_contexts")
    first = _run(_execution(1))
    assert _rerun_after_crash(MagicMock(), _plan(3, max_time=0), 0, first) == [first]
    rerun.assert_not_called()
