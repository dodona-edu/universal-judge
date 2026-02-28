"""
Evaluators for text.
"""

import math
from typing import Any

from tested.dodona import Status, StatusMessage
from tested.oracles.common import OracleConfig, OracleResult
from tested.testsuite import OutputChannel, TextOutputChannel


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


def compare_text(options: dict[str, Any], expected: str, actual: str) -> OracleResult:
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
            # noinspection PyUnboundLocalVariable
            actual_float = round(actual_float, numbers)
            expected_float = round(expected_float, numbers)
        # noinspection PyUnboundLocalVariable
        result = math.isclose(actual_float, expected_float)
        expected = str(expected_float)
    else:
        result = actual_eval == expected_eval

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
