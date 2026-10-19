"""
Support linting Python code with ruff.

The rules ruff checks (rules/ruff.toml) and the texts students see
(rules/messages.toml) are shared with judge-pythia: keep both files the same in
both judges.
"""

import json
import logging
import warnings
from pathlib import Path

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.linter import annotation_from_position, get_linter_position
from tested.judge.utils import run_command
from tested.languages.python.lint_messages import messages

logger = logging.getLogger(__name__)

RULES = Path(__file__).with_name("rules") / "ruff.toml"

# The entry in messages.toml for Python's own message about a syntax error.
SYNTAX_ERROR = "syntax-error"

# Ruff uses this instead of a rule code for syntax errors.
_INVALID_SYNTAX = "invalid-syntax"


def run_ruff(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls ruff to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source

    # A submission that doesn't compile gets one annotation with Python's own
    # message, instead of the messages of ruff's parser.
    if annotation := _syntax_error(config):
        return [], [annotation]

    language_options = config.config_for()
    if path := language_options.get("ruff_config", None):
        assert isinstance(path, str)
        config_path = config.resources / path
    else:
        # Use the default file.
        config_path = RULES
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
                get_i18n_string("languages.python.linter.timeout")
                if execution_results.timeout
                else get_i18n_string("languages.python.linter.memory")
            )
        ], []

    # Ruff exits with 0 if the submission is clean and 1 if it found something.
    # Anything else means ruff itself failed; stdout is empty then, and the
    # reason is on stderr.
    if execution_results.exit not in (0, 1):
        logger.warning("Ruff failed with %s", execution_results.stderr)
        return [
            get_i18n_string("languages.python.linter.crashed"),
            ExtendedMessage(
                description=execution_results.stderr,
                format="code",
                permission=Permission.EDUCATOR,
            ),
        ], []

    try:
        ruff_messages = json.loads(execution_results.stdout)
    except Exception as e:
        logger.warning("Ruff produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.python.linter.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.EDUCATOR
            ),
        ], []

    annotations = []

    for ruff_message in ruff_messages:
        if Path(ruff_message.get("filename", submission)).name != submission.name:
            continue

        text = ruff_message.get("message", None)
        if not text:
            continue

        code = ruff_message.get("code") or _INVALID_SYNTAX

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
                text=messages().text(code, text, config.natural_language),
                # Syntax errors have no rule, and so no documentation page.
                external_url=ruff_message.get("url", None),
                type=(
                    Severity.ERROR
                    if code == _INVALID_SYNTAX
                    else Severity(messages().type(code))
                ),
                # Ruff's messages and the texts in messages.toml mark code with
                # backticks.
                format="markdown",
            )
        )

    return [], annotations


def _syntax_error(config: DodonaConfig) -> AnnotateCode | None:
    """
    The annotation for a submission that doesn't compile, with Python's own
    message; None if it compiles.
    """
    source = config.source
    try:
        with warnings.catch_warnings():
            # Such as invalid escape sequences, which ruff reports itself.
            warnings.simplefilter("ignore")
            compile(source.read_bytes(), source.name, "exec", dont_inherit=True)
    except SyntaxError as e:
        same_line = e.end_lineno in (None, e.lineno)
        has_width = same_line and e.offset and e.end_offset and e.end_offset > e.offset
        position = get_linter_position(
            raw_start_row=e.lineno,
            source_offset=config.source_offset,
            raw_end_row=e.lineno,
            raw_start_column=e.offset,
            raw_end_column=e.end_offset if has_width else None,
            end_column_inclusive=False,
        )
        return annotation_from_position(
            position=position,
            # Plain text: Python's message doesn't mark code, so markdown could
            # garble names such as `__debug__`.
            text=messages().text(SYNTAX_ERROR, e.msg, config.natural_language),
            type=Severity.ERROR,
        )
    except (ValueError, OSError, RecursionError, MemoryError):
        # Unreadable, a null byte, or too deeply nested for the compiler (such
        # as a sum of thousands of terms): ruff reports what it can.
        return None
    return None
