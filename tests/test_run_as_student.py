"""
Tests for running the submission as the user student, which the image's
run-as-student helper makes possible.
"""

import os
import shutil
import sys
import time
from pathlib import Path

import pytest

from tested import configs
from tested.configs import (
    TESTED_DIRECTORY,
    DodonaConfig,
    create_bundle,
    runs_as_student,
)
from tested.judge.utils import (
    _give_student_access,
    read_submission_file,
    run_command,
)
from tested.testsuite import Suite
from tests.manual_utils import (
    assert_valid_output,
    configuration,
    execute_config,
    exercise_configuration,
)

needs_helper = pytest.mark.skipif(
    not runs_as_student(), reason="the image has no run-as-student helper"
)


@pytest.fixture
def fake_helper(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Path:
    helper = tmp_path / "helper" / "run-as-student"
    monkeypatch.setattr(configs, "RUN_AS_STUDENT", helper)
    return helper


def _install(helper: Path, script: str):
    helper.parent.mkdir()
    helper.write_text(f"#!/bin/sh\n{script}\n")
    helper.chmod(0o755)


def test_command_goes_through_the_helper_only_when_it_exists(
    tmp_path: Path, fake_helper: Path
):
    command = ["echo", "submission"]

    result = run_command(tmp_path, 10, command, as_student=True)
    assert result and result.stdout == "submission\n"

    _install(fake_helper, 'echo helper "$@"')
    result = run_command(tmp_path, 10, command, as_student=True)
    assert result and result.stdout == "helper echo submission\n"

    result = run_command(tmp_path, 10, command, as_student=False)
    assert result and result.stdout == "submission\n"


def test_student_command_gets_its_own_environment(
    tmp_path: Path, fake_helper: Path, monkeypatch: pytest.MonkeyPatch
):
    monkeypatch.setenv("JUDGE_ONLY", "secret")
    monkeypatch.setenv("LANG", "C.UTF-8")
    _install(fake_helper, "env")

    result = run_command(tmp_path, 10, ["submission"], as_student=True)
    assert result
    environment = result.stdout.splitlines()
    assert "LANG=C.UTF-8" in environment
    assert not any(line.startswith("JUDGE_ONLY=") for line in environment)

    result = run_command(tmp_path, 10, ["env"], as_student=False)
    assert result and "JUDGE_ONLY=secret" in result.stdout.splitlines()


def test_submission_file_is_read_through_the_helper_only_when_it_exists(
    tmp_path: Path, fake_helper: Path
):
    written = tmp_path / "written.txt"
    written.write_text("by the submission\r\n")
    missing = tmp_path / "missing.txt"

    assert read_submission_file(written) == "by the submission\n"
    with pytest.raises(FileNotFoundError):
        read_submission_file(missing)

    _install(fake_helper, 'echo "read by $1 through the helper"')
    assert read_submission_file(written) == "read by cat through the helper\n"
    with pytest.raises(FileNotFoundError):
        read_submission_file(missing)


def test_submission_file_is_a_directory_only_when_it_is_one(
    tmp_path: Path, fake_helper: Path
):
    directory = tmp_path / "directory"
    directory.mkdir()
    link_to_directory = tmp_path / "link-to-directory"
    link_to_directory.symlink_to(directory)
    link_to_nothing = tmp_path / "link-to-nothing"
    link_to_nothing.symlink_to(tmp_path / "missing")

    with pytest.raises(IsADirectoryError):
        read_submission_file(directory)

    _install(fake_helper, 'echo "read by $1 through the helper"')
    with pytest.raises(IsADirectoryError):
        read_submission_file(directory)
    # A link goes to the helper, whatever it points to.
    for link in (link_to_directory, link_to_nothing):
        assert read_submission_file(link) == "read by cat through the helper\n"


def test_student_command_does_not_get_the_judges_stdin(
    tmp_path: Path, fake_helper: Path
):
    _install(fake_helper, 'exec "$@"')
    judge_stdin = os.dup(0)
    try:
        for as_student, expected in ((True, ""), (False, "the judge's input\n")):
            read_end, write_end = os.pipe()
            os.write(write_end, b"the judge's input\n")
            os.close(write_end)
            os.dup2(read_end, 0)
            os.close(read_end)
            result = run_command(tmp_path, 10, ["cat"], as_student=as_student)
            assert result and result.stdout == expected
    finally:
        os.dup2(judge_stdin, 0)
        os.close(judge_stdin)


def test_tested_directory_leaves_the_workdir_when_running_as_student(
    tmp_path: Path, fake_helper: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo", "python", tmp_path, "one.tson", "correct"
    )

    bundle = create_bundle(conf, sys.stdout, Suite())
    assert bundle.global_config.tested_directory == tmp_path / TESTED_DIRECTORY

    _install(fake_helper, "")
    bundle = create_bundle(conf, sys.stdout, Suite())
    tested_directory = bundle.global_config.tested_directory
    assert tested_directory.parent == Path("/tmp")
    assert not tested_directory.is_relative_to(tmp_path)
    assert not tested_directory.exists()


def test_custom_check_creates_no_directory_of_its_own(
    tmp_path: Path,
    fake_helper: Path,
    pytestconfig: pytest.Config,
    monkeypatch: pytest.MonkeyPatch,
):
    _install(fake_helper, 'exec "$@"')
    root = tmp_path / "tmp"
    root.mkdir()
    monkeypatch.setattr(configs, "STUDENT_DIRECTORY_ROOT", root)
    workdir = tmp_path / "workdir"
    workdir.mkdir()
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        workdir,
        "one-programmed-correct.tson",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(list(root.iterdir())) == 1


def _mode(path: Path) -> int:
    return path.stat().st_mode & 0o777


def test_student_access_is_added_to_the_judges_files_only(tmp_path: Path):
    directory = tmp_path / "unit"
    directory.mkdir(mode=0o755)
    file = directory / "file.txt"
    file.write_text("judge")
    file.chmod(0o644)
    subdirectory = directory / "subdirectory"
    subdirectory.mkdir(mode=0o755)
    nested = subdirectory / "nested.txt"
    nested.write_text("judge")
    nested.chmod(0o644)
    private = tmp_path / "private"
    private.mkdir(mode=0o700)
    secret = private / "secret.txt"
    secret.write_text("judge only")
    secret.chmod(0o600)
    (directory / "link-to-file").symlink_to(secret)
    (directory / "link-to-directory").symlink_to(private)
    os.mkfifo(directory / "pipe", 0o600)

    _give_student_access(directory)

    assert _mode(directory) == 0o777
    assert _mode(subdirectory) == 0o777
    assert _mode(file) == 0o666
    assert _mode(nested) == 0o666
    assert _mode(private) == 0o700
    assert _mode(secret) == 0o600
    assert _mode(directory / "pipe") == 0o600


@pytest.mark.skipif(os.getuid() != 0, reason="giving a file away needs root")
def test_student_access_skips_files_of_another_user(tmp_path: Path):
    directory = tmp_path / "unit"
    directory.mkdir()
    other = directory / "other.txt"
    other.write_text("not the judge's")
    other.chmod(0o600)
    os.chown(other, 1001, 1001)

    _give_student_access(directory)

    assert _mode(other) == 0o600


def _private_isolation_exercise(
    tmp_path: Path, pytestconfig: pytest.Config, suite: str
) -> DodonaConfig:
    """The isolation exercise, with the judge's files private like Dodona's mount."""
    exercise = pytestconfig.rootpath / "tests" / "exercises" / "isolation"
    resources = tmp_path / "resources"
    shutil.copytree(exercise / "evaluation", resources)
    judge = tmp_path / "judge"
    templates = Path("tested") / "languages" / "python" / "templates"
    shutil.copytree(pytestconfig.rootpath / templates, judge / templates)
    suite_file = resources / suite
    suite_file.write_text(
        suite_file.read_text()
        .replace("<resources>", str(resources))
        .replace("<judge>", str(judge))
    )
    workdir = tmp_path / "workdir"
    workdir.mkdir()
    return exercise_configuration(
        pytestconfig,
        exercise,
        "python",
        workdir,
        suite,
        "probes",
        {"resources": resources, "judge": judge},
    )


@needs_helper
def test_submission_cannot_reach_the_judge(tmp_path: Path, pytestconfig: pytest.Config):
    conf = _private_isolation_exercise(tmp_path, pytestconfig, "suite.yaml")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 9
    assert not (conf.judge / "tested" / "probe.txt").exists()


@needs_helper
def test_output_file_linked_to_a_judge_file_is_not_read(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = _private_isolation_exercise(tmp_path, pytestconfig, "link.yaml")
    result = execute_config(conf)
    assert "judge only" not in result
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["internal error"]


@needs_helper
def test_timeout_as_student_ends_the_judgement(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "isolation",
        "python",
        tmp_path,
        "timeout.yaml",
        "probes",
        {"time_limit": 10},
    )
    start = time.monotonic()
    result = execute_config(conf)
    elapsed = time.monotonic() - start

    updates = assert_valid_output(result, pytestconfig)
    # The test's own status, then the judgement's.
    assert updates.find_status_enum() == ["time limit exceeded"] * 2
    assert updates[-1]["command"] == "close-judgement"
    assert elapsed < 60


def test_output_file_written_through_a_temporary_file(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function-file-output",
        "python",
        tmp_path,
        "one.yaml",
        "temporary-file",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
