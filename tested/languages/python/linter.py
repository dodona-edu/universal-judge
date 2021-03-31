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
from tested.internationalization import get_i18n_string

logger = logging.getLogger(__name__)

message_categories = {
    'fatal':      Severity.ERROR,
    'error':      Severity.ERROR,
    'warning':    Severity.WARNING,
    'convention': Severity.INFO,
    'refactor':   Severity.INFO
}


def run_pylint(bundle: Bundle, submission: Path, remaining: float) \
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
        config_path = config.judge / "tested/languages/python/pylint_config.rc"

    pylint_out = StringIO()
    try:
        args = [f"--rcfile={config_path}", str(submission)]
        logger.debug("Running with template_args %s", args)
        lint.Run(args, reporter=JSONReporter(output=pylint_out), do_exit=False)
    except Exception as e:
        logger.warning("Pylint crashed with", exc_info=e)
        return [get_i18n_string("languages.python.linter.crashed"), ExtendedMessage(
            description=str(e),
            format='code',
            permission=Permission.STAFF
        )], []

    try:
        messages = json.loads(pylint_out.getvalue())
    except Exception as e:
        logger.warning("Pylint produced bad output", exc_info=e)
        return [get_i18n_string("languages.python.linter.output"), ExtendedMessage(
            description=str(e),
            format='code',
            permission=Permission.STAFF
        )], []

    annotations = []

    for message in messages:
        category = message_categories.get(message.get("type", "warning"),
                                          Severity.WARNING)
        logger.debug("Handling message %s", str(message))
        message_id = message.get('message-id', None)
        message_text = message.get('message', None)
        if not message_id and not message_text:
            continue
        elif not message_id:
            text = message_text
        elif not message_text:
            text = f"({message_id})"
        else:
            text = f"{message_text} ({message_id})"
        annotations.append(AnnotateCode(
            row=max(int(message.get("line", "-1")) - 1, 0),
            column=max(int(message.get("column", "-1")) - 1, 0),
            text=text,
            type=category
        ))

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    # for now, reports are not processed
    return [], annotations
