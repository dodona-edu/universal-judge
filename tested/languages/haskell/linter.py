import json
import logging
from pathlib import Path

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.linter import annotation_from_position, get_linter_position
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)

message_categories = {
    "Error": Severity.ERROR,
    "Warning": Severity.WARNING,
    "Suggestion": Severity.INFO,
}


def run_hlint(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls eslint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source
    language_options = config.config_for()
    if path := language_options.get("hlint_config", None):
        assert isinstance(path, str)
        config_path = config.resources / path
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/haskell/hlint.yml"
    config_path = str(config_path.absolute())

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=[
            "hlint",
            "-j",
            "--json",
            "-h",
            config_path,
            str(submission.absolute()),
        ],
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            (
                get_i18n_string("languages.haskell.linter.timeout")
                if execution_results.timeout
                else get_i18n_string("languages.haskell.linter.memory")
            )
        ], []

    try:
        hlint_messages = json.loads(execution_results.stdout)
    except Exception as e:
        logger.warning("HLint produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.haskell.linter.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
        ], []
    annotations = []

    for hlint_message in hlint_messages:
        if Path(hlint_message.get("file", submission)).name != submission.name:
            continue
        notes = "\n".join(hlint_message.get("note", []))
        hint = hlint_message.get("hint", None)
        if not hint:
            continue

        hint_from = hlint_message.get("from", None)
        hint_to = hlint_message.get("to", None)
        if hint_from and hint_to:
            hint = f"{hint}\nfrom: `{hint_from}`\nto: `{hint_to}`"
        if notes:
            hint = f"{hint}\n{notes}"

        position = get_linter_position(
            raw_start_row=hlint_message.get("startLine"),
            source_offset=config.source_offset,
            raw_end_row=hlint_message.get("endLine"),
            raw_start_column=hlint_message.get("startColumn"),
            raw_end_column=hlint_message.get("endColumn"),
            end_column_inclusive=False,
        )

        annotations.append(
            annotation_from_position(
                position=position,
                text=hint,
                external_url="https://github.com/ndmitchell/hlint/blob/master/hints.md",
                type=message_categories.get(
                    hlint_message.get("severity", "warning"), Severity.WARNING
                ),
            ),
        )

    return [], annotations
