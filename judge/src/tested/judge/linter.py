import logging
import shutil
from pathlib import Path

from ..configs import Bundle
from ..dodona import report_update, AppendMessage

_logger = logging.getLogger(__name__)


def run_linter(bundle: Bundle):
    """
    Run the linter on the submission. For the linter to run, two preconditions
    must be satisfied:

    1. The programming language supports a linter.
    2. The linter is allowed to run based on the configuration.

    :param bundle: The configuration bundle.
    """

    language_options = bundle.plan.config_for(bundle.config.programming_language)
    # By default, we allow the linter to work.
    if not language_options.get("linter", False):
        _logger.debug("Linter is disabled.")
        return

    _logger.debug("Running linter...")

    messages, annotations = \
        bundle.language_config.run_linter(bundle, bundle.config.source)

    for message in messages:
        report_update(bundle.out, AppendMessage(message=message))
    for annotation in annotations:
        report_update(bundle.out, annotation)
