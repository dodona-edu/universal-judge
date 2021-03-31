import json
import logging
from pathlib import Path
from typing import Tuple, List

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Message, ExtendedMessage, Permission, \
    Severity
from tested.internationalization import get_i18n_string
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)
severity = [Severity.INFO, Severity.WARNING, Severity.ERROR]


def run_eslint(bundle: Bundle, submission: Path, remaining: float) \
        -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls eslint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    config = bundle.config
    language_options = bundle.config.config_for()
    if language_options.get("eslint_config", None):
        config_path = config.resources / language_options.get('eslint_config')
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/javascript/eslintrc.yml"
    config_path = config_path.absolute()

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=["eslint", "-f", "json", "--no-inline-config", "-c", config_path,
                 submission.absolute()]
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [get_i18n_string(
            "languages.javascript.linter.timeout") if execution_results.timeout else
                get_i18n_string("languages.javascript.linter.memory")], []

    try:
        eslint_objects = json.loads(execution_results.stdout)
    except Exception as e:
        logger.warning("ESLint produced bad output", exc_info=e)
        return [get_i18n_string("languages.javascript.linter.output"),
                ExtendedMessage(
                    description=str(e),
                    format='code',
                    permission=Permission.STAFF
                )], []
    annotations = []

    for eslint_object in eslint_objects:
        if Path(eslint_object.get('filePath', submission)).name != submission.name:
            continue
        for message in eslint_object.get('messages', []):
            text = message.get('message', None)
            if not text:
                continue
            annotations.append(AnnotateCode(
                row=max(int(message.get('line', "-1")) - 1, 0),
                text=text,
                column=max(int(message.get('column', "-1")) - 1, 0),
                type=severity[int(message.get('severity', 1))],
            ))

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
