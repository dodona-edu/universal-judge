"""
Support linting Python code.
Most of this code is taken from the code from Pythia.
"""

import logging
from io import StringIO

from pylint import lint
from pylint.reporters import JSONReporter

from tested.configs import DodonaConfig
from tested.dodona import *
from tested.internationalization import get_i18n_string

logger = logging.getLogger(__name__)

message_categories = {
    "fatal": Severity.ERROR,
    "error": Severity.ERROR,
    "warning": Severity.WARNING,
    "convention": Severity.INFO,
    "refactor": Severity.INFO,
}


def run_pylint(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls pylint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source
    language_options = config.config_for()
    if path := language_options.get("pylint_config", None):
        assert isinstance(path, str)
        config_path = config.resources / path
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/python/pylint_config.rc"

    pylint_out = StringIO()
    try:
        args = [f"--rcfile={config_path}", str(submission)]
        logger.debug("Running with template_args %s", args)
        lint.Run(args, reporter=JSONReporter(output=pylint_out), exit=False)
    except Exception as e:
        logger.warning("Pylint crashed with", exc_info=e)
        return [
            get_i18n_string("languages.python.linter.crashed"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
        ], []

    try:
        messages = json.loads(pylint_out.getvalue())
    except Exception as e:
        logger.warning("Pylint produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.python.linter.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
        ], []

    annotations = []

    for message in messages:
        category = message_categories.get(
            message.get("type", "warning"), Severity.WARNING
        )
        logger.debug("Handling message %s", str(message))
        text = message.get("message", None)
        symbol = message.get("symbol", None)
        raw_type = message.get("type", None)

        external = None
        if not symbol and not text:
            continue
        if symbol and raw_type:
            external = (
                f"https://pylint.pycqa.org/en/latest/messages/{raw_type}/{symbol}.html"
            )

        start_row = message.get("line", 0)
        end_row = message.get("endLine")
        rows = end_row + 1 - start_row if end_row else None
        start_col = message.get("column", 0)
        end_col = message.get("endColumn")
        # Prevent negative columns
        if rows == 1:
            cols = end_col - start_col if end_col else None
        else:
            cols = end_col

        annotations.append(
            AnnotateCode(
                row=start_row - 1 + config.source_offset,
                rows=rows,
                column=start_col,
                columns=cols,
                text=text,
                externalUrl=external,
                type=category,
            )
        )

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    # for now, reports are not processed
    return [], annotations
