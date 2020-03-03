"""
Support linting Python code.
Most of this code is taken from the code from Pythia.
"""
import logging
import shutil
from os import PathLike
from pathlib import Path
from typing import List, Tuple
from io import StringIO

from pylint import lint
from pylint.reporters import JSONReporter

from dodona import *
from tested import Config

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


def run_pylint(config: Config, path: Path, submission: Path) \
        -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls pylint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    pylint_config: Union[Path, PathLike] = path / 'pylint_config.rc'
    if config.options.get("custom_pylint"):
        # If a custom pylint config exists, use it.
        source = f"{config.resources}/{config.options['custom_pylint']}"
        shutil.copy2(source, pylint_config)
    else:
        # Use the default file.
        source = f"{config.judge}/judge/runners/languages/pylint_config.rc"
        shutil.copy2(source, pylint_config)

    pylint_out = StringIO()
    try:
        args = [f"--rcfile={pylint_config}", submission]
        logger.debug("Running with args %s", args)
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


def run_linter(config: Config, path: Path, submission: Union[Path, PathLike]) \
        -> Tuple[List[Message], List[AnnotateCode]]:
    logger.debug("Running py_lint...")
    # run pylint to collect evaluation score and annotations
    return run_pylint(config, path, submission)
