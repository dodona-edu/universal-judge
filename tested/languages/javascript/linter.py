import json
import logging
from pathlib import Path

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)
severity = [Severity.INFO, Severity.WARNING, Severity.ERROR]


def run_eslint(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls eslint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source
    language_options = config.config_for()
    if path := language_options.get("eslint_config", None):
        assert isinstance(path, str)
        config_path = config.resources / path
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/javascript/eslintrc.yml"
    config_path = str(config_path.absolute())

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=[
            "eslint",
            "-f",
            "json",
            "--no-inline-config",
            "-c",
            config_path,
            str(submission.absolute()),
        ],
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            get_i18n_string("languages.javascript.linter.timeout")
            if execution_results.timeout
            else get_i18n_string("languages.javascript.linter.memory")
        ], []

    try:
        eslint_objects = json.loads(execution_results.stdout)
    except Exception as e:
        logger.warning("ESLint produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.javascript.linter.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
        ], []
    annotations = []

    for eslint_object in eslint_objects:
        if Path(eslint_object.get("filePath", submission)).name != submission.name:
            continue
        for message in eslint_object.get("messages", []):
            text = message.get("message", None)
            if not text:
                continue
            rule_id = message.get("ruleId")
            external = None
            if rule_id:
                external = f"https://eslint.org/docs/rules/{rule_id}"

            start_row = message.get("line", 1)
            end_row = message.get("endLine")
            rows = end_row - start_row if end_row and end_row > start_row else None
            start_col = message.get("column", 1)
            end_col = message.get("endColumn")
            cols = end_col - start_col if end_col and end_col > start_col else None
            annotations.append(
                AnnotateCode(
                    row=start_row - 1 + config.source_offset,
                    rows=rows,
                    text=text,
                    externalUrl=external,
                    column=start_col - 1,
                    columns=cols,
                    type=severity[int(message.get("severity", 1))],
                )
            )

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
