"""
Tests for how the result of the compilation step is turned into a status.
"""

from unittest.mock import MagicMock

import pytest
from pytest_mock import MockerFixture

from tested.dodona import Status
from tested.judge.compilation import process_compile_results
from tested.judge.utils import BaseExecutionResult


def compilation(**kwargs) -> BaseExecutionResult:
    defaults = {
        "stdout": "",
        "stderr": "",
        "exit": 0,
        "timeout": False,
        "memory": False,
    }
    return BaseExecutionResult(**(defaults | kwargs))


@pytest.fixture
def language(mocker: MockerFixture) -> MagicMock:
    mocker.patch(
        "tested.judge.compilation.convert_stacktrace_to_clickable_feedback",
        side_effect=lambda config, text: text,
    )
    config = MagicMock()
    config.compiler_output.side_effect = lambda stdout, stderr: ([], [], stdout, stderr)
    return config


def test_no_compilation_step_is_correct(language: MagicMock):
    assert process_compile_results(language, None).status == Status.CORRECT


def test_successful_compilation_is_correct(language: MagicMock):
    result = process_compile_results(language, compilation())
    assert result.status == Status.CORRECT


def test_failing_compiler_is_a_compilation_error(language: MagicMock):
    result = process_compile_results(
        language, compilation(exit=1, stderr="error: expected ';'")
    )
    assert result.status == Status.COMPILATION_ERROR
    assert "error: expected ';'" in result.messages


def test_silent_failing_compiler_reports_its_exit_code(language: MagicMock):
    result = process_compile_results(language, compilation(exit=1))
    assert result.status == Status.COMPILATION_ERROR
    assert "1" in str(result.messages[0])


def test_compiler_killed_for_memory_reports_the_memory_limit(language: MagicMock):
    # The cgroup OOM killer sends SIGKILL, which run_command reports as exit -9.
    result = process_compile_results(language, compilation(exit=-9, memory=True))
    assert result.status == Status.MEMORY_LIMIT_EXCEEDED
    assert result.messages


def test_compilation_timeout_reports_the_time_limit(language: MagicMock):
    # run_command reports a timeout with exit code 0.
    result = process_compile_results(language, compilation(timeout=True))
    assert result.status == Status.TIME_LIMIT_EXCEEDED
    assert result.messages
