import json
import logging
from pathlib import Path

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)


def run_ktlint(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls ktlint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source
    language_options = config.config_for()

    command = ["ktlint", "--reporter=json", "--log-level=error"]

    if path := language_options.get("editorconfig", None):
        assert isinstance(path, str)
        command.append(f"--editorconfig={config.resources / path}")

    if language_options.get("disabled_rules_ktlint", None):
        rules = language_options["disabled_rules_ktlint"]
        if isinstance(rules, list):
            rules = ",".join(rules)
        if "filename" not in rules:
            rules += ",filename"
        command.append(f"--disabled_rules={rules}")
    else:
        command.append("--disabled_rules=filename")

    if path := language_options.get("ktlint_ruleset", None):
        assert isinstance(path, str)
        command.append(f"--ruleset={config.resources / path}")

    if language_options.get("ktlint_experimental", True):
        command.append("--experimental")

    submission = submission.absolute()

    command.append(str(submission.relative_to(submission.parent)))

    execution_results = run_command(
        directory=submission.parent, timeout=remaining, command=command
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            get_i18n_string("languages.kotlin.linter.timeout")
            if execution_results.timeout
            else get_i18n_string("languages.kotlin.linter.memory")
        ], []

    try:
        ktlint_objects = json.loads(execution_results.stdout)
    except Exception as e:
        logger.warning("KTLint produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.kotlin.linter.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
            ExtendedMessage(
                description=execution_results.stdout,
                format="code",
                permission=Permission.STAFF,
            ),
        ], []
    annotations = []

    for ktlint_object in ktlint_objects:
        if Path(ktlint_object.get("file", submission)).name != submission.name:
            continue
        for error in ktlint_object.get("errors", []):
            message = error.get("message", None)
            if not message:
                continue
            rule = error.get("rule", None)
            if rule:
                message += f"({rule})"

            annotations.append(
                AnnotateCode(
                    row=error.get("line", 1) - 1 + config.source_offset,
                    text=message,
                    externalUrl="https://ktlint.github.io/#rules",
                    column=error.get("column", 1) - 1,
                    type=Severity.INFO,
                )
            )

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
