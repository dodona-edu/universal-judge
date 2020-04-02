"""
Evaluators for text.
"""
from typing import Optional, Dict, Any

import math

from ..dodona import StatusMessage, Status
from . import EvaluationResult, EvaluatorConfig
from ..testplan import TextOutputChannel, FileOutputChannel, OutputChannel


def _is_number(string: str) -> Optional[float]:
    try:
        return float(string)
    except ValueError:
        return None


def _text_options(config: EvaluatorConfig) -> dict:
    defaults = {
        # Options for textual comparison
        'ignoreWhitespace': True,
        'caseInsensitive':  False,
        # Options for numerical comparison
        'tryFloatingPoint': False,
        'applyRounding':    False,
        'roundTo':          3
    }
    defaults.update(config.options)
    return defaults


def _file_defaults(config: EvaluatorConfig) -> dict:
    defaults = {
        "mode": "exact"
    }
    defaults.update(config.options)
    if defaults["mode"] not in {"exact", "lines", "values"}:
        raise ValueError(f"Unknown mode for file evaluator: {defaults['mode']}")
    return defaults


def compare_text(
        options: Dict[str, Any],
        expected: str,
        actual: str, wrong: Status) -> EvaluationResult:
    if options['ignoreWhitespace']:
        expected = expected.strip()
        actual = actual.strip()

    if options['caseInsensitive']:
        expected = expected.lower()
        actual = actual.lower()

    if options['tryFloatingPoint'] and (actual_float := _is_number(actual)):
        expected_float = float(expected)
        if options['applyRounding']:
            numbers = int(options['roundTo'])
            # noinspection PyUnboundLocalVariable
            actual_float = round(actual_float, numbers)
            expected_float = round(expected_float, numbers)
        # noinspection PyUnboundLocalVariable
        result = math.isclose(actual_float, expected_float)
    else:
        result = actual == expected

    return EvaluationResult(
        result=StatusMessage(enum=Status.CORRECT if result else wrong),
        readable_expected=str(expected),
        readable_actual=str(actual)
    )


def evaluate_text(
        config: EvaluatorConfig,
        channel: OutputChannel,
        actual: str, wrong: Status, timeout: Optional[float]) -> EvaluationResult:
    """
    The base evaluator, used to compare two strings. As this evaluator is
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
    return compare_text(options, expected, actual, wrong)


def evaluate_file(config: EvaluatorConfig,
                  channel: OutputChannel,
                  actual: str, wrong: Status,
                  timeout: Optional[float]) -> EvaluationResult:
    """
    Evaluate the contents of two files. The file evaluator supports one option,
    ``mode``, used to define in which mode the evaluator should operate:

    1. ``exact``: Both files must be exactly the same, including line separators.
    2. ``lines``: Each line is evaluated against the corresponding line in the
                  other file. The evaluation itself is exact.
    3. ``values``: Each line is evaluated as a textual value, using the
                   :class:`TextEvaluator`. In this mode, options from this
                   evaluator are passed to the text evaluator.

    When no mode is passed, the evaluator will default to exact.
    """
    assert isinstance(channel, FileOutputChannel)
    options = _text_options(config)

    # There must be nothing as output.
    if actual:
        message = f"Verwachtte geen uitvoer voor kanaal, maar vond {actual}."
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human="Onverwachte uitvoer."
            ),
            readable_expected="",
            readable_actual=actual,
            messages=[message]
        )

    expected_path = f"{config.bundle.config.resources}/{channel.expected_path}"

    try:
        with open(expected_path, "r") as file:
            expected = file.read()
    except FileNotFoundError:
        raise ValueError(f"File {expected_path} not found in resources.")

    actual_path = config.context_dir / channel.actual_path

    try:
        with open(str(actual_path), "r") as file:
            actual = file.read()
    except FileNotFoundError:
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.RUNTIME_ERROR,
                human="Bestand niet gevonden."
            ),
            readable_expected=expected,
            readable_actual="",
        )

    if options["mode"] == "exact":
        result = StatusMessage(
            enum=Status.CORRECT if actual == expected else wrong
        )
    elif options["mode"] == "lines":
        expected_lines = expected.splitlines()
        actual_lines = actual.splitlines()
        correct = len(actual_lines) == len(expected_lines)
        for (expected_line, actual_line) in zip(expected_lines, actual_lines):
            correct = correct and expected_line == actual_line
        result = StatusMessage(enum=Status.CORRECT if correct else wrong)
    else:
        assert options["mode"] == "values"
        expected_lines = expected.splitlines()
        actual_lines = actual.splitlines()
        correct = len(actual_lines) == len(expected_lines)
        # Overwrite the channel and actual with the values from the text
        # evaluator. This will give a more consistent output in Dodona.
        expected = []
        actual = []
        for (expected_line, actual_line) in zip(expected_lines, actual_lines):
            r = compare_text(options, expected_line, actual_line, wrong)
            expected.append(r.readable_expected)
            actual.append(r.readable_actual)
            correct = correct and r.result.enum == Status.CORRECT
        result = StatusMessage(enum=Status.CORRECT if correct else wrong)

    return EvaluationResult(
        result=StatusMessage(enum=Status.CORRECT if result else wrong),
        readable_expected=expected,
        readable_actual=actual
    )
