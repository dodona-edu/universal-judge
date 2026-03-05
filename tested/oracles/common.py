"""
Oracles compare values to determine the result of a test.

## Implementing oracles

An oracle is just a function that receives some configuration parameters and
returns a result.

The following parameters are passed to the function:

- RawOracle configs, consisting of:
  - The global configuration for the run of TESTed
  - The configuration for the oracle instance
  - The judge instance
- The output channel from the test suite.
- The raw actual output.
- The maximum time for the oracle. Simple oracles can ignore this, but more
  advanced ones need more time.

For example, such a function looks like this:

   def evaluate_text(configs, channel, actual):
        pass
"""

import functools
from collections.abc import Callable
from pathlib import Path
from typing import Any

from attrs import define, field

from tested.configs import Bundle
from tested.dodona import Message, Status, StatusMessage
from tested.dsl import parse_string
from tested.languages.generation import generate_statement
from tested.languages.utils import convert_stacktrace_to_clickable_feedback
from tested.parsing import fallback_field, get_converter
from tested.serialisation import Value
from tested.testsuite import (
    ExceptionOutputChannel,
    NormalOutputChannel,
    OutputChannel,
    SupportedLanguage,
)


@define
class OracleContext:
    expected: Value
    actual: Value
    execution_directory: Path
    evaluation_directory: Path
    submission_path: Path | None
    programming_language: SupportedLanguage
    natural_language: str


@define
class OracleResult:
    """
    Represents the result of applying an oracle to evaluate some result.
    """

    result: StatusMessage
    """
    The result of the evaluation.
    """
    readable_expected: str
    """
    A human-friendly version of what the channel should have been.
    """
    readable_actual: str
    """
    A human-friendly version (on a best-efforts basis) of what the channel is.
    """
    messages: list[Message] = field(factory=list)
    is_multiline_string: bool = False
    """
    Indicates if the evaluation result is a multiline string.
    """
    channel_override: str | None = None
    """
    Allows overriding as which channel this will be reported.
    """


@fallback_field(
    {
        "readableExpected": "readable_expected",
        "readableActual": "readable_actual",
        "dslExpected": "dsl_expected",
        "dslActual": "dsl_actual",
    },
)
@define
class BooleanEvalResult:
    """
    Allows a boolean result.

    Note: this class is used directly in the Python oracle, so keep it backwards
    compatible (also positional arguments) or make a new class for the oracle.
    """

    result: bool | Status
    readable_expected: str | None = None
    readable_actual: str | None = None
    messages: list[Message] = field(factory=list)
    dsl_expected: str | None = None
    dsl_actual: str | None = None

    def to_oracle_result(
        self,
        bundle: Bundle,
        channel: NormalOutputChannel,
        fallback_actual: str,
        fallback_expected: str,
    ) -> OracleResult:
        if isinstance(self.result, Status):
            status = self.result
        else:
            status = Status.CORRECT if self.result else Status.WRONG

        if self.readable_expected:
            readable_expected = self.readable_expected
        elif self.dsl_expected:
            parsed_statement = parse_string(self.dsl_expected, True)
            readable_expected = generate_statement(bundle, parsed_statement)
        else:
            readable_expected = fallback_expected
        if self.readable_actual:
            readable_actual = self.readable_actual
        elif self.dsl_actual:
            parsed_statement = parse_string(self.dsl_actual, True)
            readable_actual = generate_statement(bundle, parsed_statement)
        else:
            readable_actual = fallback_actual
        messages = self.messages

        if isinstance(channel, ExceptionOutputChannel):
            readable_expected = bundle.language.cleanup_stacktrace(readable_expected)
            message = convert_stacktrace_to_clickable_feedback(
                bundle.language, readable_actual
            )
            if message:
                messages.append(message)

            if status == Status.CORRECT:
                readable_actual = ""

        return OracleResult(
            result=StatusMessage(enum=status),
            readable_expected=readable_expected,
            readable_actual=readable_actual,
            messages=messages,
        )


@define
class OracleConfig:
    bundle: Bundle
    options: dict[str, Any]
    context_dir: Path


RawOracle = Callable[
    [OracleConfig, OutputChannel, str], OracleResult | list[OracleResult]
]

Oracle = Callable[[OutputChannel, str], OracleResult | list[OracleResult]]


def _curry_oracle(
    bundle: Bundle,
    context_dir: Path,
    function: RawOracle,
    options: dict | None = None,
) -> Oracle:
    if options is None:
        options = dict()
    config = OracleConfig(bundle, options, context_dir)
    # noinspection PyTypeChecker
    return functools.partial(function, config)


def try_outputs(
    actual: str, parsers: list[Callable[[str], tuple[str | None, Message | None]]]
) -> tuple[str, Message | None]:
    if not actual:
        return actual, None
    for parser in parsers:
        possible, msg = parser(actual)
        if possible is not None:
            return possible, msg
    return actual, None
