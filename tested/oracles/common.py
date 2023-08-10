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
from pathlib import Path
from typing import Any, Callable, Dict, List, NamedTuple, Optional, Tuple

from attrs import define, field

from tested.configs import Bundle
from tested.dodona import Message, Status, StatusMessage
from tested.languages.utils import convert_stacktrace_to_clickable_feedback
from tested.serialisation import EvalResult
from tested.testsuite import ExceptionOutputChannel, NormalOutputChannel, OutputChannel


@define
class OracleResult:
    """
    Represents the result of applying an oracle to evaluate some result.
    """

    result: StatusMessage  # The result of the evaluation.
    readable_expected: str  # A human-friendly version of what the channel should have been.
    readable_actual: str  # A human-friendly version (on a best-efforts basis) of what the channel is.
    messages: List[Message] = field(factory=list)
    is_multiline_string: bool = (
        False  # Indicates if the evaluation result is a multiline string.
    )


class OracleConfig(NamedTuple):
    bundle: Bundle
    options: Dict[str, Any]
    context_dir: Path


RawOracle = Callable[[OracleConfig, OutputChannel, str], OracleResult]

Oracle = Callable[[OutputChannel, str], OracleResult]


def _curry_oracle(
    bundle: Bundle,
    context_dir: Path,
    function: RawOracle,
    options: Optional[dict] = None,
) -> Oracle:
    if options is None:
        options = dict()
    config = OracleConfig(bundle, options, context_dir)
    # noinspection PyTypeChecker
    return functools.partial(function, config)


def try_outputs(
    actual: str, parsers: List[Callable[[str], Tuple[Optional[str], Optional[Message]]]]
) -> Tuple[str, Optional[Message]]:
    if not actual:
        return actual, None
    for parser in parsers:
        possible, msg = parser(actual)
        if possible is not None:
            return possible, msg
    return actual, None


def cleanup_specific_programmed(
    config: OracleConfig, channel: NormalOutputChannel, actual: EvalResult
) -> EvalResult:
    if isinstance(channel, ExceptionOutputChannel):
        lang_config = config.bundle.lang_config
        actual.readable_expected = lang_config.cleanup_stacktrace(
            actual.readable_expected or ""
        )
        message = convert_stacktrace_to_clickable_feedback(
            lang_config, actual.readable_actual
        )

        if message:
            actual.messages.append(message)

        if actual.result == Status.CORRECT:
            actual.readable_actual = ""

    return actual
