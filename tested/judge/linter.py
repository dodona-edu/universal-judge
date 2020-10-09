import logging

from .collector import OutputManager
from ..configs import Bundle
from ..dodona import AppendMessage

_logger = logging.getLogger(__name__)


def runs_linter(bundle: Bundle) -> bool:
    return bundle.config.linter()


def run_linter(bundle: Bundle, collector: OutputManager, remaining: float):
    """
    Run the linter on the submission. For the linter to run, two preconditions
    must be satisfied:

    1. The programming language supports a linter.
    2. The linter is allowed to run based on the configuration.

    :param bundle: The configuration bundle.
    :param collector: The output collector.
    :param remaining: The remaining time for the execution.
    """

    if not runs_linter(bundle):
        _logger.debug("Linter is disabled.")
        return

    _logger.debug("Running linter...")

    messages, annotations = \
        bundle.lang_config.linter(bundle, bundle.config.source, remaining)

    for message in messages:
        collector.add(AppendMessage(message=message))
    for annotation in annotations:
        collector.add(annotation)
