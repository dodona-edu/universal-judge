"""
Support linting Python code.
Most of this code is taken from the code from Pythia.
"""
import logging
import shutil
from io import StringIO
from os import PathLike
from pathlib import Path
from typing import List, Tuple

from pylint import lint
from pylint.reporters import JSONReporter

from tested.configs import Bundle
from tested.dodona import *

logger = logging.getLogger(__name__)

# localize judge output
translations = {
    'en': {
        'linting tab':     'code',
        'TLE description': 'time limit exceeded',
    },
    'nl': {
        'linting tab':     'code',
        'TLE description': 'tijdslimiet overschreden',
    },
}

message_categories = {
    'fatal':      Severity.ERROR,
    'error':      Severity.ERROR,
    'warning':    Severity.WARNING,
    'convention': Severity.INFO,
    'refactor':   Severity.INFO
}


def run_pylint(bundle: Bundle, submission: Path, remaining: int) \
        -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls pylint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    config = bundle.config
    language_options = bundle.config.config_for()
    if language_options.get("pylint_config", None):
        config_path = config.resources / language_options.get('pylint_config')
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/configs/pylint_config.rc"

    pylint_out = StringIO()
    try:
        args = [f"--rcfile={config_path}", str(submission)]
        logger.debug("Running with template_args %s", args)
        lint.Run(args, reporter=JSONReporter(output=pylint_out), do_exit=False)
    except Exception as e:
        logger.warning("Pylint crashed with", exc_info=e)
        return ["Pylint crashed", ExtendedMessage(
            description=str(e),
            format='code',
            permission=Permission.STAFF
        )], []

    try:
        messages = json.loads(pylint_out.getvalue())
    except Exception as e:
        logger.warning("Pylint produced bad output", exc_info=e)
        return ["Pylint produced bad output.", ExtendedMessage(
            description=str(e),
            format='code',
            permission=Permission.STAFF
        )], []

    annotations = []

    for message in messages:
        category = message_categories.get(message["type"], Severity.WARNING)
        logger.debug("Handling message %s", str(message))
        annotations.append(AnnotateCode(
            row=max(int(message["line"]) - 1, 0),
            column=max(int(message["column"]) - 1, 0),
            text=f"{message['message']} ({message['message-id']})",
            type=category
        ))

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    # for now, reports are not processed
    return [], annotations
