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
    if not language_options.get("linter", True):
        _logger.debug("Linter is disabled.")
        return

    directory = Path(bundle.config.workdir) / "linter"
    directory.mkdir()

    _logger.debug("Running linter in %s", directory)

    # Copy the submission.
    source_path = shutil.copy2(bundle.config.source, directory)
    _logger.debug("Copying %s to linter dir", bundle.config.source)

    messages, annotations = \
        bundle.language_config.run_linter(bundle.config, directory, source_path)

    for message in messages:
        report_update(bundle.out, AppendMessage(message=message))
    for annotation in annotations:
        report_update(bundle.out, annotation)
