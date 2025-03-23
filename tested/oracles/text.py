"""
Evaluators for text.
"""

import math
import os
from typing import Any

from tested.dodona import Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.judge.utils import base64_encode
from tested.oracles.common import OracleConfig, OracleResult
from tested.testsuite import (
    FileOutputChannel,
    OutputChannel,
    OutputFileData,
    TextChannelType,
    TextOutputChannel,
)


def _is_number(string: str) -> float | None:
    try:
        return float(string)
    except ValueError:
        return None


def _text_options(config: OracleConfig) -> dict:
    defaults = {
        # Options for textual comparison
        "ignoreWhitespace": False,
        "caseInsensitive": False,
        # Options for numerical comparison
        "tryFloatingPoint": False,
        "applyRounding": False,
        "roundTo": 3,
        # This option is used in the DSL, no in the actual oracle.
        "normalizeTrailingNewlines": True,
    }
    defaults.update(config.options)
    return defaults


def _file_defaults(config: OracleConfig) -> dict:
    defaults = {"mode": "exact"}
    defaults.update(config.options)
    if defaults["mode"] not in ("exact", "lines", "values"):
        raise ValueError(f"Unknown mode for file oracle: {defaults['mode']}")
    return defaults


def _text_comparison(
    options: dict[str, Any], expected: str, actual: str
) -> tuple[bool, str]:
    # Temporary variables that may modified by the evaluation options,
    # Don't modify the actual values, otherwise there maybe confusion with the
    # solution submitted by the student
    expected_eval, actual_eval = str(expected), str(actual)

    if options["ignoreWhitespace"]:
        expected_eval, actual_eval = expected_eval.rstrip(), actual_eval.rstrip()

    if options["caseInsensitive"]:
        expected_eval, actual_eval = expected_eval.lower(), actual_eval.lower()

    if (
        options["tryFloatingPoint"]
        and (actual_float := _is_number(actual_eval.strip())) is not None
    ):
        expected_float = float(expected_eval.strip())
        if options["applyRounding"]:
            numbers = int(options["roundTo"])
            actual_float = round(actual_float, numbers)
            expected_float = round(expected_float, numbers)
        return math.isclose(actual_float, expected_float), str(expected_float)

    return actual_eval == expected_eval, expected


def compare_text(options: dict[str, Any], expected: str, actual: str) -> OracleResult:

    result, expected = _text_comparison(options, expected, actual)

    return OracleResult(
        result=StatusMessage(enum=Status.CORRECT if result else Status.WRONG),
        readable_expected=str(expected),
        readable_actual=str(actual),
    )


def evaluate_text(
    config: OracleConfig, channel: OutputChannel, actual: str
) -> OracleResult:
    """
    The base oracle, used to compare two strings. As this oracle is
    intended for evaluating stdout, it supports various options to make life
    easier:

    - ``ignoreWhitespace``: whitespace before and after will be stripped
    - ``caseInsensitive``: all comparisons will be in lower-case
    - ``tryFloatingPoint``: try to evaluate_text the value as a floating-point
    - ``applyRounding``: limit floating points to ``roundTo`` numbers
    - ``roundTo``: amount of numbers to round to.

    Note: floating points inside other texts are currently not supported.
    """
    assert isinstance(channel, TextOutputChannel)
    options = _text_options(config)

    expected = channel.get_data_as_string(config.bundle.config.resources)
    result = compare_text(options, expected, actual)
    return result


def make_expected_and_actual_file_output(
    output_data: OutputFileData, expected: str, actual: str
) -> tuple[str, str]:
    content_type = output_data.content_type
    expected_str = output_data.content
    if content_type == TextChannelType.TEXT:
        expected_str = base64_encode(expected)

    return (
        f"--- <{output_data.student_path}|{content_type}> ---\n{expected_str}",
        f"--- <{output_data.student_path}|text> ---\n{base64_encode(actual)}",
    )


def evaluate_file(
    config: OracleConfig, channel: OutputChannel, actual: str
) -> OracleResult:
    """
    Evaluate the contents of two files. The file oracle supports one option,
    ``mode``, used to define in which mode the oracle should operate:

    1. ``full``: The complete contents are passed to the :class:`TextEvaluator`.
    2. ``line``: The file is split by lines and each line is compared to the
       corresponding line with the :class:`TextEvaluator`. The lines are compared
       without newlines.

    Since the text oracle is used behind the scenes, this oracle also supports
    all parameters of that oracle.

    When no mode is passed, the oracle will default to ``full``.
    """
    assert isinstance(channel, OutputFileData)
    options = _text_options(config)

    # There must be nothing as output.
    if actual:
        message = get_i18n_string("oracles.text.file.unexpected.message", actual=actual)
        return OracleResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human=get_i18n_string("oracles.text.file.unexpected.status"),
            ),
            readable_expected="",
            readable_actual=actual,
            messages=[message],
        )

    expected = channel.content
    if channel.content_type == TextChannelType.FILE:
        expected_path = f"{config.bundle.config.resources}/{expected}"

        try:
            with open(expected_path, "r") as file:
                expected = file.read()
        except FileNotFoundError:
            raise ValueError(f"File {expected_path} not found in resources.")

    actual_path = config.context_dir / channel.student_path

    try:
        with open(str(actual_path), "r") as file:
            actual = file.read()
    except FileNotFoundError:
        return OracleResult(
            result=StatusMessage(
                enum=Status.RUNTIME_ERROR,
                human=get_i18n_string("oracles.text.file.not-found"),
            ),
            readable_expected=expected,
            readable_actual="",
        )

    if options["mode"] == "full":
        return compare_text(options, expected, actual)
    else:
        assert options["mode"] == "line"
        strip_newlines = options.get("stripNewlines", False)
        expected_lines = expected.splitlines(keepends=not strip_newlines)
        actual_lines = actual.splitlines(keepends=not strip_newlines)
        correct = len(actual_lines) == len(expected_lines)
        for expected_line, actual_line in zip(expected_lines, actual_lines):
            r = compare_text(options, expected_line, actual_line)
            correct = correct and r.result.enum == Status.CORRECT
        return OracleResult(
            result=StatusMessage(enum=Status.CORRECT if correct else Status.WRONG),
            readable_expected=expected,
            readable_actual=actual,
        )
