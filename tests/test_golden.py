"""
Golden tests: judge every submission in tests/golden and compare TESTed's complete
output with the expected file next to it.

Layout of tests/golden/<exercise>/:
    evaluation/suite.yaml                    the test suite (+ files it needs)
    config.json                              optional judge options, merged in; its
                                             "languages" object overrides them
                                             per language
    submissions/<language>/<case>.<ext>      one test per submission
    expected/<language>/<case>.json          the golden output

Run with UPDATE_EXPECTED=1 to write the expected files instead of comparing them.
Cases listed in tests/golden/known-failures.txt are strict xfails.
"""

import difflib
import json
import os
import warnings
from pathlib import Path
from typing import Any

import pytest

from tests.manual_utils import execute_config, exercise_configuration

GOLDEN_DIR = Path(__file__).parent / "golden"
UPDATE_EXPECTED = os.environ.get("UPDATE_EXPECTED") == "1"


def _known_failures() -> dict[str, str]:
    lines = (GOLDEN_DIR / "known-failures.txt").read_text().splitlines()
    entries = [line.split() for line in lines if line and not line.startswith("#")]
    return {case: " ".join(urls) for case, *urls in entries}


KNOWN_FAILURES = _known_failures()


def _cases() -> list:
    cases = []
    for submission in sorted(GOLDEN_DIR.glob("*/submissions/*/*")):
        language = submission.parent.name
        exercise = submission.parent.parent.parent.name
        case_id = f"{exercise}/{language}/{submission.stem}"
        marks = []
        if case_id in KNOWN_FAILURES:
            marks.append(pytest.mark.xfail(strict=True, reason=KNOWN_FAILURES[case_id]))
        cases.append(
            pytest.param(exercise, language, submission, id=case_id, marks=marks)
        )
    return cases


def _parse_output(output: str) -> list[dict[str, Any]]:
    decoder = json.JSONDecoder()
    commands = []
    position = 0
    while position < len(output):
        if output[position].isspace():
            position += 1
            continue
        command, position = decoder.raw_decode(output, position)
        commands.append(command)
    return commands


def _normalise(output: str, workdir: Path, judge: Path) -> list[dict[str, Any]]:
    """
    The only rules that make two runs of the same case comparable:
    - the per-test workdir becomes <workdir>, the judge directory becomes <judge>.
    """
    output = output.replace(f"{workdir}/", "<workdir>/")
    output = output.replace(f"{judge}/", "<judge>/")
    return _parse_output(output)


TOOLCHAIN_PLACEHOLDER = "<toolchain text>"
TOOLCHAIN_CHANNELS = ("exception", "stderr")


def _is_toolchain_message(message: dict[str, Any]) -> bool:
    if message.get("format") == "code":
        return True
    return message.get("format") == "html" and message["description"].startswith(
        "<div class='code'>"
    )


def _split_toolchain_text(
    commands: list[dict[str, Any]],
) -> tuple[list[dict[str, Any]], list[str]]:
    """
    Toolchain text is text a compiler, runtime or interpreter wrote, which may change
    with the image. It is exactly:
    - the description of an append-message whose message has format "code", or format
      "html" starting with <div class='code'> (TESTed's rendering of compiler output
      and stack traces);
    - the generated text of a close-test on the exception or stderr channel.
    Everything else is strict. Returns the commands with the toolchain text replaced
    by a placeholder, and the toolchain texts in order.
    """
    strict = []
    texts = []
    channel = None
    for command in commands:
        name = command["command"]
        message = command.get("message")
        if name == "start-test":
            channel = command.get("channel")
        if (
            name == "append-message"
            and isinstance(message, dict)
            and _is_toolchain_message(message)
        ):
            texts.append(message["description"])
            message = {**message, "description": TOOLCHAIN_PLACEHOLDER}
            command = {**command, "message": message}
        elif name == "close-test" and channel in TOOLCHAIN_CHANNELS:
            texts.append(command["generated"])
            command = {**command, "generated": TOOLCHAIN_PLACEHOLDER}
        strict.append(command)
    return strict, texts


def _dump(commands: list[dict[str, Any]]) -> str:
    return json.dumps(commands, indent=1, sort_keys=True, ensure_ascii=False) + "\n"


def test_known_failures_exist():
    case_ids = {case.id for case in _cases()}
    assert set(KNOWN_FAILURES) <= case_ids


@pytest.mark.parametrize("exercise, language, submission", _cases())
def test_golden(
    exercise: str,
    language: str,
    submission: Path,
    tmp_path: Path,
    pytestconfig: pytest.Config,
):
    case_id = f"{exercise}/{language}/{submission.stem}"
    if UPDATE_EXPECTED and case_id in KNOWN_FAILURES:
        pytest.skip("known failure: its expected file is written by hand")
    exercise_dir = GOLDEN_DIR / exercise
    expected_file = exercise_dir / "expected" / language / f"{submission.stem}.json"
    options: dict[str, Any] = {"natural_language": "en", "source": submission}
    if (config_file := exercise_dir / "config.json").is_file():
        config = json.loads(config_file.read_text())
        per_language = config.pop("languages", {})
        options |= config | per_language.get(language, {})
    workdir = tmp_path / "workdir"
    workdir.mkdir()
    conf = exercise_configuration(
        pytestconfig,
        exercise_dir,
        language,
        workdir,
        "suite.yaml",
        submission.stem,
        options,
    )

    actual = _normalise(execute_config(conf), workdir, pytestconfig.rootpath)

    if UPDATE_EXPECTED:
        expected_file.parent.mkdir(parents=True, exist_ok=True)
        expected_file.write_text(_dump(actual))
        return

    assert (
        expected_file.is_file()
    ), f"{expected_file} is missing: run with UPDATE_EXPECTED=1 to write it."
    expected = json.loads(expected_file.read_text())

    strict_actual, actual_texts = _split_toolchain_text(actual)
    strict_expected, expected_texts = _split_toolchain_text(expected)
    assert _dump(strict_actual) == _dump(strict_expected)
    for expected_text, actual_text in zip(expected_texts, actual_texts):
        if expected_text != actual_text:
            diff = difflib.unified_diff(
                expected_text.replace("<br>", "\n").splitlines(),
                actual_text.replace("<br>", "\n").splitlines(),
                "expected",
                "actual",
                lineterm="",
            )
            warnings.warn("toolchain text differs:\n" + "\n".join(diff))
