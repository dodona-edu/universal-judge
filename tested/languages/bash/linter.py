import json
import logging
import shutil
from pathlib import Path

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.linter import annotation_from_position, get_linter_position
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)
message_categories = {
    "error": Severity.ERROR,
    "warning": Severity.WARNING,
    "info": Severity.INFO,
    "style": Severity.INFO,
}


def run_shellcheck(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls shellcheck to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source
    language_options = config.config_for()
    if path := language_options.get("shellcheck_config", None):
        assert isinstance(path, str)
        config_path = config.resources / path
        # Add shellcheck file in home folder
        shutil.copy2(config_path, Path.home() / ".shellcheckrc")

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=[
            "shellcheck",
            "-f",
            "json1",
            "-s",
            "bash",
            str(submission.absolute()),
        ],
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            (
                get_i18n_string("languages.bash.linter.timeout")
                if execution_results.timeout
                else get_i18n_string("languages.bash.linter.memory")
            )
        ], []

    try:
        shellcheck_objects = json.loads(execution_results.stdout)
    except Exception as e:
        logger.warning("ShellCheck produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.bash.linter.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
        ], []
    annotations = []
    shellcheck_objects = shellcheck_objects.get("comments", [])

    for shellcheck_object in shellcheck_objects:
        if Path(shellcheck_object.get("file", submission)).name != submission.name:
            continue
        text = shellcheck_object.get("message", "")
        code = shellcheck_object.get("code", None)
        if not text and not code:
            continue
        external = None
        if code:
            external = f"https://github.com/koalaman/shellcheck/wiki/SC{code}"

        position = get_linter_position(
            raw_start_row=shellcheck_object.get("line"),
            source_offset=config.source_offset,
            raw_end_row=shellcheck_object.get("endLine"),
            raw_start_column=shellcheck_object.get("column"),
            raw_end_column=shellcheck_object.get("endColumn"),
            end_column_inclusive=False,
        )

        annotations.append(
            annotation_from_position(
                position=position,
                text=text,
                type=message_categories.get(
                    shellcheck_object.get("level", "warning"), Severity.WARNING
                ),
                external_url=external,
            )
        )

    return [], annotations
