import json
import logging
import shutil
from pathlib import Path
from typing import Tuple, List

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Message, ExtendedMessage, Permission, \
    Severity
from tested.internationalization import get_i18n_string
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)
message_categories = {
    'error':   Severity.ERROR,
    'warning': Severity.WARNING,
    'info':    Severity.INFO,
    'style':   Severity.INFO
}


def run_shellcheck(bundle: Bundle, submission: Path, remaining: float,
                   language: str = "bash") \
        -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls shellcheck to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    config = bundle.config
    language_options = bundle.config.config_for()
    if language_options.get("shellcheck_config", None):
        config_path = config.resources / language_options.get('shellcheck_config')
        # Add shellcheck file in home folder
        shutil.copy2(Path(config_path), Path(Path.home(), ".shellcheckrc"))

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=["shellcheck", "-f", "json", "-s", language, submission.absolute()]
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [get_i18n_string(
            "languages.bash.linter.timeout") if execution_results.timeout else
                get_i18n_string("languages.bash.linter.memory")], []

    try:
        shellcheck_objects = json.loads(execution_results.stdout)
    except Exception as e:
        logger.warning("ShellCheck produced bad output", exc_info=e)
        return [get_i18n_string("languages.bash.linter.output"),
                ExtendedMessage(
                    description=str(e),
                    format='code',
                    permission=Permission.STAFF
                )], []
    annotations = []

    for shellcheck_object in shellcheck_objects:
        if Path(shellcheck_object.get('file', submission)).name != submission.name:
            continue
        text = shellcheck_object.get('message', None)
        code = shellcheck_object.get('code', None)
        if not text and code is None:
            continue
        elif not text:
            text = f'(code {code})'
        elif code is not None:
            text = f'{text} (code {code})'
        annotations.append(AnnotateCode(
            row=max(int(shellcheck_object.get('line', "-1")) - 1, 0),
            text=text,
            column=max(int(shellcheck_object.get('column', "-1")) - 1, 0),
            type=message_categories.get(shellcheck_object.get('level', "warning"),
                                        Severity.WARNING),
            externalUrl=f'https://github.com/koalaman/shellcheck/wiki/SC{code}'
        ))

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
