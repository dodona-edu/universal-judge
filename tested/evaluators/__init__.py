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
from pathlib import Path
from typing import Callable, Optional, Union

from tested.configs import Bundle
from tested.dodona import Status
from tested.evaluators.common import Evaluator, RawEvaluator, _curry_evaluator
from tested.testsuite import (
    EmptyChannel,
    ExceptionBuiltin,
    ExitCodeOutputChannel,
    GenericExceptionEvaluator,
    GenericTextEvaluator,
    GenericValueEvaluator,
    IgnoredChannel,
    NormalOutputChannel,
    ProgrammedEvaluator,
    SpecialOutputChannel,
    SpecificEvaluator,
    TextBuiltin,
    ValueBuiltin,
)


def get_evaluator(
    bundle: Bundle,
    context_dir: Path,
    output: Union[NormalOutputChannel, SpecialOutputChannel],
    unexpected_status: Status = Status.WRONG,
) -> Evaluator:
    """
    Get the evaluator for a given output channel.
    """
    from ..evaluators import (
        exception,
        exitcode,
        ignored,
        nothing,
        programmed,
        specific,
        text,
        value,
    )

    currier: Callable[[RawEvaluator, Optional[dict]], Evaluator] = functools.partial(
        _curry_evaluator, bundle, context_dir
    )

    # Handle channel states.
    if output == EmptyChannel.NONE:
        evaluator = functools.partial(
            nothing.evaluate, unexpected_status=unexpected_status
        )
        return currier(evaluator)
    if output == IgnoredChannel.IGNORED:
        return currier(ignored.evaluate)
    if isinstance(output, ExitCodeOutputChannel):
        return currier(exitcode.evaluate)

    assert hasattr(output, "evaluator")

    # Handle actual evaluators.
    evaluator = output.evaluator

    # Handle built-in text evaluators
    if isinstance(evaluator, GenericTextEvaluator):
        if evaluator.name == TextBuiltin.TEXT:
            return currier(text.evaluate_text, evaluator.options)
        elif evaluator.name == TextBuiltin.FILE:
            return currier(text.evaluate_file, evaluator.options)
        raise AssertionError("Unknown built-in text evaluator")
    # Handle built-in value evaluators
    elif isinstance(evaluator, GenericValueEvaluator):
        assert evaluator.name == ValueBuiltin.VALUE
        return currier(value.evaluate, evaluator.options)
    # Handle built-in exception evaluators
    elif isinstance(evaluator, GenericExceptionEvaluator):
        assert evaluator.name == ExceptionBuiltin.EXCEPTION
        return currier(exception.evaluate, evaluator.options)
    # Handle programmed evaluators
    elif isinstance(evaluator, ProgrammedEvaluator):
        return currier(programmed.evaluate)
    elif isinstance(evaluator, SpecificEvaluator):
        return currier(specific.evaluate)
    else:
        raise AssertionError(f"Unknown evaluator type: {type(evaluator)}")
