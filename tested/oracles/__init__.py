import functools
from collections.abc import Callable
from pathlib import Path

from tested.configs import Bundle
from tested.dodona import Status
from tested.oracles import file
from tested.oracles.common import Oracle, RawOracle, _curry_oracle
from tested.testsuite import (
    CustomCheckOracle,
    EmptyChannel,
    ExceptionBuiltin,
    ExitCodeOutputChannel,
    GenericExceptionOracle,
    GenericTextOracle,
    GenericValueOracle,
    IgnoredChannel,
    LanguageSpecificOracle,
    NormalOutputChannel,
    SpecialOutputChannel,
    Testcase,
    TextBuiltin,
    ValueBuiltin,
)


def get_oracle(
    bundle: Bundle,
    context_dir: Path,
    output: NormalOutputChannel | SpecialOutputChannel,
    testcase: Testcase | None = None,
    unexpected_status: Status = Status.WRONG,
) -> Oracle:
    """
    Get the oracle for a given output channel.
    """
    from ..oracles import (
        exception,
        exitcode,
        ignored,
        nothing,
        programmed,
        specific,
        text,
        value,
    )

    currier: Callable[[RawOracle, dict | None], Oracle] = functools.partial(
        _curry_oracle, bundle, context_dir
    )

    # Handle channel states.
    if output == EmptyChannel.NONE:
        oracle = functools.partial(
            nothing.evaluate, unexpected_status=unexpected_status, testcase=testcase
        )
        return currier(oracle)
    if output == IgnoredChannel.IGNORED:
        return currier(ignored.evaluate)
    if isinstance(output, ExitCodeOutputChannel):
        return currier(exitcode.evaluate)

    assert hasattr(output, "oracle")

    # Handle actual functions.
    oracle = output.oracle

    # Handle built-in text functions
    if isinstance(oracle, GenericTextOracle):
        if oracle.name == TextBuiltin.TEXT:
            return currier(text.evaluate_text, oracle.options)
        elif oracle.name == TextBuiltin.FILE:
            return currier(file.evaluate_file, oracle.options)
        raise AssertionError("Unknown built-in text oracle")
    # Handle built-in value functions
    elif isinstance(oracle, GenericValueOracle):
        assert oracle.name == ValueBuiltin.VALUE
        return currier(value.evaluate, oracle.options)
    # Handle built-in exception functions
    elif isinstance(oracle, GenericExceptionOracle):
        assert oracle.name == ExceptionBuiltin.EXCEPTION
        return currier(exception.evaluate, oracle.options)
    # Handle programmed functions
    elif isinstance(oracle, CustomCheckOracle):
        return currier(programmed.evaluate)
    elif isinstance(oracle, LanguageSpecificOracle):
        return currier(specific.evaluate)
    else:
        raise AssertionError(f"Unknown oracle type: {type(oracle)}")
