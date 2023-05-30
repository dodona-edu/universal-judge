"""
Evaluators actually compare values to determine the result of a test.

## Implementing an evaluator

An evaluator is just a function that receives some configuration parameters and
returns a result.

The following parameters are passed to the function:

- RawEvaluator configs, consisting of:
  - The global configuration for the run of TESTed
  - The configuration for the evaluator instance
  - The judge instance
- The output channel from the test suite.
- The raw actual output.
- The maximum time for the evaluation. Simple evaluators can ignore this, but more
  advanced ones need more time.

For example, such a function looks like this:

   def evaluate_text(configs, channel, actual):
        pass
"""
import functools
from dataclasses import field
from pathlib import Path
from typing import Any, Callable, Dict, List, NamedTuple, Optional, Tuple, Union

from pydantic.dataclasses import dataclass

from tested.configs import Bundle
from tested.dodona import Message, Status, StatusMessage
from tested.languages.utils import convert_stacktrace_to_clickable_feedback
from tested.serialisation import EvalResult
from tested.testsuite import ExceptionOutputChannel, NormalOutputChannel, OutputChannel


@dataclass
class EvaluationResult:
    """Provides the result of an evaluation for a specific output channel."""

    result: StatusMessage  # The result of the evaluation.
    readable_expected: str
    """
    A human-friendly version of what the channel should have been.
    """
    readable_actual: str
    """
    A human-friendly version (on a best-efforts basis) of what the channel is.
    """
    messages: List[Message] = field(default_factory=list)
    is_multiline_string: bool = False
    """
    Indicates if the evaluation result is a multiline string
    """


class EvaluatorConfig(NamedTuple):
    bundle: Bundle
    options: Dict[str, Any]
    context_dir: Path


RawEvaluator = Callable[[EvaluatorConfig, OutputChannel, str], EvaluationResult]

Evaluator = Callable[[OutputChannel, str], EvaluationResult]


def _curry_evaluator(
    bundle: Bundle,
    context_dir: Path,
    function: RawEvaluator,
    options: Optional[dict] = None,
) -> Evaluator:
    if options is None:
        options = dict()
    config = EvaluatorConfig(bundle, options, context_dir)
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


def get_status(status: Optional[Union[bool, Status]]) -> Status:
    if status is None:
        return Status.WRONG
    elif isinstance(status, bool):
        return Status.CORRECT if status else Status.WRONG
    else:
        return status


def cleanup_specific_programmed(
    config: EvaluatorConfig, channel: NormalOutputChannel, actual: EvalResult
) -> EvalResult:
    actual.result = get_status(actual.result)
    if isinstance(channel, ExceptionOutputChannel):
        lang_config = config.bundle.lang_config
        actual.readable_expected = lang_config.cleanup_stacktrace(
            actual.readable_expected
        )
        message = convert_stacktrace_to_clickable_feedback(
            lang_config, actual.readable_actual
        )

        if message:
            actual.messages.append(message)

        if actual.result == Status.CORRECT:
            actual.readable_actual = ""

    return actual
