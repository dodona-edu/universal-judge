"""
Evaluators for text.
"""

import math
from typing import Any

from tested.dodona import Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.oracles.common import OracleConfig, OracleResult
from tested.testsuite import (
    FileOutputChannel,
    OutputChannel,
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
    assert isinstance(channel, FileOutputChannel)
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

    actual_list = []
    expected_list = []
    file_not_found = False
    for i in range(len(channel.content)):
        actual_path = config.context_dir / channel.path[i]

        if channel.content_type[i] == TextChannelType.FILE:
            expected_path = f"{config.bundle.config.resources}/{channel.content[i]}"
            try:
                with open(expected_path, "r") as file:
                    expected_list.append(file.read())
            except FileNotFoundError:
                raise ValueError(f"File {expected_path} not found in resources.")
        else:
            expected_list.append(channel.content[i])

        try:
            with open(str(actual_path), "r") as file:
                actual_list.append(file.read())
        except FileNotFoundError:
            file_not_found = True

    actual = "\n".join(actual_list)
    expected = "\n".join(expected_list)
    if file_not_found:
        return OracleResult(
            result=StatusMessage(
                enum=Status.RUNTIME_ERROR,
                human=get_i18n_string("oracles.text.file.not-found"),
            ),
            readable_expected=expected,
            readable_actual=actual,
        )

    result = True

    if options["mode"] == "full":
        for i in range(len(expected_list)):
            expected_value = expected_list[i]
            actual_value = actual_list[i]
            new_result, expected_list[i] = _text_comparison(
                options, expected_value, actual_value
            )
            result = result and new_result
    else:
        assert options["mode"] == "line"
        for i in range(len(expected_list)):
            expected_value = expected_list[i]
            actual_value = actual_list[i]
            strip_newlines = options.get("stripNewlines", False)
            expected_lines = expected_value.splitlines(keepends=not strip_newlines)
            actual_lines = actual_value.splitlines(keepends=not strip_newlines)
            result = len(actual_lines) == len(expected_lines)
            for expected_line, actual_line in zip(expected_lines, actual_lines):
                print(f"{expected_line}: {actual_line}")
                new_result, _ = _text_comparison(options, expected_line, actual_line)
                print(f"{new_result}")
                result = result and new_result

    return OracleResult(
        result=StatusMessage(enum=Status.CORRECT if result else Status.WRONG),
        readable_expected="\n".join(expected_list),
        readable_actual=actual,
    )
