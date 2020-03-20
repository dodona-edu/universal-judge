import logging
from typing import Optional

from tested.dodona import StatusMessage, Status
from evaluators import EvaluationResult
from testplan.channels import ExitCodeOutputChannel

logger = logging.getLogger(__name__)


def _as_int(value: str) -> Optional[int]:
    try:
        return int(value)
    except (ValueError, TypeError):
        return None


def evaluate(_, channel: ExitCodeOutputChannel, value: str) -> EvaluationResult:
    assert isinstance(channel, ExitCodeOutputChannel)
    exit_code = _as_int(value)

    if exit_code is None:
        logger.warning(f"Could not interpret {exit_code} as a valid exit code.")
        return EvaluationResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human=f"Ongeldige exitcode {exit_code}."
            ),
            readable_expected=str(channel.value),
            readable_actual=str(value),
            messages=[
                f"Kon {exit_code} niet interpreteren als exitcode."
            ]
        )

    expected_exit_code = channel.value

    if expected_exit_code != exit_code:
        status = StatusMessage(enum=Status.WRONG, human="Verkeerde exitcode.")
    else:
        status = StatusMessage(enum=Status.CORRECT)

    return EvaluationResult(
        result=status,
        readable_expected=str(expected_exit_code),
        readable_actual=str(exit_code)
    )
