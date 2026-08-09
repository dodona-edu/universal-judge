"""
Support linting Python code with ruff.

This is the linter that is actually used; the pylint implementation next to this
one is kept around while we compare both.
"""

import json
import logging
from pathlib import Path

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.linter import annotation_from_position, get_linter_position
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)

# Ruff does report a severity for every violation, but it is "error" for all of
# them, so we derive the severity from the rule code instead. The mapping mirrors
# how pylint categorised things: broken code is an error, suspicious code a
# warning, and stylistic suggestions are informational.
_ERROR_PREFIXES = ("E9", "F70", "F81", "F82", "PLE")
_WARNING_PREFIXES = ("ARG", "B", "BLE", "F", "PLW", "S", "SLF", "TRY")

# Ruff uses this instead of a rule code for syntax errors.
_SYNTAX_ERROR = "invalid-syntax"


def _severity(code: str) -> Severity:
    if code == _SYNTAX_ERROR or code.startswith(_ERROR_PREFIXES):
        return Severity.ERROR
    # A bare except was a warning in pylint (W0702); the other pycodestyle rules
    # we select were conventions.
    if code == "E722" or code.startswith(_WARNING_PREFIXES):
        return Severity.WARNING
    return Severity.INFO


def run_ruff(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls ruff to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source
    language_options = config.config_for()
    if path := language_options.get("ruff_config", None):
        assert isinstance(path, str)
        config_path = config.resources / path
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/python/ruff.toml"
    config_path = str(config_path.absolute())

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=[
            "ruff",
            "check",
            # Passing a config file also stops ruff from picking up a stray
            # pyproject.toml or ruff.toml next to the submission.
            "--config",
            config_path,
            "--output-format",
            "json",
            # Don't leave a .ruff_cache behind in the submission directory.
            "--no-cache",
            "--quiet",
            str(submission.absolute()),
        ],
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            (
                get_i18n_string("languages.python.ruff.timeout")
                if execution_results.timeout
                else get_i18n_string("languages.python.ruff.memory")
            )
        ], []

    # Ruff exits with 0 if the submission is clean and 1 if it found something.
    # Anything else means ruff itself failed; stdout is empty then, and the
    # reason is on stderr.
    if execution_results.exit not in (0, 1):
        logger.warning("Ruff failed with %s", execution_results.stderr)
        return [
            get_i18n_string("languages.python.ruff.crashed"),
            ExtendedMessage(
                description=execution_results.stderr,
                format="code",
                permission=Permission.STAFF,
            ),
        ], []

    try:
        ruff_messages = json.loads(execution_results.stdout)
    except Exception as e:
        logger.warning("Ruff produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.python.ruff.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
        ], []

    annotations = []

    for ruff_message in ruff_messages:
        if Path(ruff_message.get("filename", submission)).name != submission.name:
            continue

        text = ruff_message.get("message", None)
        if not text:
            continue

        code = ruff_message.get("code") or ""
        if code and code != _SYNTAX_ERROR:
            text = f"{text} ({code})"

        location = ruff_message.get("location") or {}
        end_location = ruff_message.get("end_location") or {}
        start_row = location.get("row")
        end_row = end_location.get("row")
        end_column = end_location.get("column")

        # Ruff's end position is exclusive, so a range that stops at the very
        # start of a line does not cover that line. Without this, every syntax
        # error would highlight one line too many.
        if end_row is not None and end_column == 1 and end_row != start_row:
            end_row -= 1
            end_column = None

        position = get_linter_position(
            raw_start_row=start_row,
            source_offset=config.source_offset,
            raw_end_row=end_row,
            raw_start_column=location.get("column"),
            raw_end_column=end_column,
            end_column_inclusive=False,
        )

        annotations.append(
            annotation_from_position(
                position=position,
                text=text,
                # Syntax errors have no rule, and so no documentation page.
                external_url=ruff_message.get("url", None),
                type=_severity(code),
            )
        )

    return [], annotations
