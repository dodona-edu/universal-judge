import logging
import os
from pathlib import Path
from typing import Tuple, List
from xml.etree import ElementTree

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
    'ignore':  Severity.INFO
}

ENV_CHECKSTYLE = "CHECKSTYLE_JAR"


def run_checkstyle(bundle: Bundle, submission: Path, remaining: float) \
        -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls checkstyle to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    config = bundle.config
    language_options = bundle.config.config_for()

    if ENV_CHECKSTYLE not in os.environ:
        return [ExtendedMessage(
            description=get_i18n_string("languages.linter.not-found",
                                        linter="Checkstyle"),
            format='text',
            permission=Permission.STAFF
        )], []

    checkstyle_jar = Path(os.environ[ENV_CHECKSTYLE])

    if language_options.get("checkstyle_config", None):
        config_path = config.resources / language_options.get('checkstyle_config')
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/java/sun_tested_checks.xml"
    config_path = config_path.absolute()

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=["java", "-jar", checkstyle_jar, "-f", "xml", "-c", config_path,
                 submission.name]
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [get_i18n_string(
            "languages.java.linter.timeout") if execution_results.timeout else
                get_i18n_string("languages.java.linter.memory")], []

    try:
        xml_tree = ElementTree.fromstring(execution_results.stdout)
    except Exception as e:
        logger.warning("Checkstyle produced bad output", exc_info=e)
        return [get_i18n_string("languages.java.linter.output"),
                ExtendedMessage(
                    description=str(e),
                    format='code',
                    permission=Permission.STAFF
                )], []
    annotations = []

    for file_tree in xml_tree:
        if Path(file_tree.attrib.get('name', submission)).name != submission.name:
            continue
        for error_element in file_tree:
            message = error_element.attrib.get('message', None)
            if not message:
                continue
            source = error_element.attrib.get('source', None)
            if source:
                more_info = get_i18n_string("languages.linter.more-info")
                message += f' (https://checkstyle.sourceforge.io/apidocs/' \
                           f'index.html?{source.replace(".", "/")}.html" ' \
                           f'target="_blank">{more_info}</a>)'
            annotations.append(AnnotateCode(
                row=max(int(error_element.attrib.get('line', "-1")) - 1, 0),
                text=message,
                column=max(int(error_element.attrib.get('column', "-1")) - 1, 0),
                type=message_categories.get(
                    error_element.attrib.get('severity', "warning"),
                    Severity.WARNING),
            ))

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
