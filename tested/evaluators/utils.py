from typing import Optional, Union

from tested.testplan import NormalOutputChannel, ExceptionOutputChannel
from . import EvaluatorConfig
from ..dodona import Status
from ..serialisation import EvalResult


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
        namespace = lang_config.conventionalize_namespace(config.bundle.plan.namespace)
        actual.readable_expected = lang_config.cleanup_stacktrace(
            actual.readable_expected, lang_config.with_extension(namespace)
        )
        cleaned_actual = lang_config.cleanup_stacktrace(
            actual.readable_actual, lang_config.with_extension(namespace)
        )
        message = lang_config.clean_stacktrace_to_message(cleaned_actual)

        if message:
            actual.messages.append(message)

        if actual.result == Status.CORRECT:
            actual.readable_actual = ""

    return actual
