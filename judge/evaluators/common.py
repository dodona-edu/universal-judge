"""Defines the interface evaluators should follow."""
from dataclasses import dataclass, field
from typing import List, Dict, Any

from dodona.common import Status, Message
from dodona.partial_output import StatusMessage


@dataclass
class EvaluationResult:
    """Provides the result of an evaluation for a specific output channel."""
    result: StatusMessage  # The result of the evaluation.
    readable_expected: str  # A human-friendly version of what the channel should have been.
    readable_actual: str  # A human-friendly version (best effort at least) of what the channel is.
    messages: List[Message] = field(default_factory=list)


class Evaluator:
    """Base evaluator containing minimal functionality."""

    def __init__(self, arguments: Dict[str, Any] = None):
        self.arguments = arguments or {}

    def evaluate(self, expected, actual) -> EvaluationResult:
        """
        Evaluate the result.
        :param expected: The expected result, extracted from the testplan.
        :param actual: The actual result, produced by the user's code.
        :return: The result of the evaluation.
        """
        raise NotImplementedError
