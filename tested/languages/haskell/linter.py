import json
import logging
from pathlib import Path
from typing import List, Tuple

from tested.configs import Bundle
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)

message_categories = {
    "Error": Severity.ERROR,
    "Warning": Severity.WARNING,
    "Suggestion": Severity.INFO,
}


def run_hlint(
    bundle: Bundle, submission: Path, remaining: float
) -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls eslint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    config = bundle.config
    language_options = bundle.config.config_for()
    if language_options.get("hlint_config", None):
        config_path = config.resources / language_options.get("hlint_config")
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/haskell/hlint.yml"
    config_path = config_path.absolute()

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=["hlint", "-j", "--json", "-h", config_path, submission.absolute()],
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            get_i18n_string("languages.haskell.linter.timeout")
            if execution_results.timeout
            else get_i18n_string("languages.haskell.linter.memory")
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

        start_row = hlint_message.get("startLine", 1)
        end_row = hlint_message.get("endLine")
        rows = end_row - start_row if end_row else None
        start_col = hlint_message.get("startColumn", 1)
        end_col = hlint_message.get("endColumn")
        cols = end_col - start_col if end_col else None

        annotations.append(
            AnnotateCode(
                row=start_row - 1 + bundle.config.source_offset,
                rows=rows,
                text=hint,
                externalUrl="https://github.com/ndmitchell/hlint/blob/master/hints.md",
                column=start_col - 1,
                columns=cols,
                type=message_categories.get(
                    hlint_message.get("severity", "warning"), Severity.WARNING
                ),
            )
        )
    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
