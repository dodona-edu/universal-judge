import logging
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


def run_checkstyle(bundle: Bundle, submission: Path, remaining: float) \
        -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls checkstyle to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    config = bundle.config
    language_options = bundle.config.config_for()

    checkstyle_jar = config.judge / "tested/languages/java/checkstyle-8.41-all.jar"
    checkstyle_jar = checkstyle_jar.absolute()

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
        if Path(file_tree.attrib['name']).name != submission.name:
            continue
        for error_element in file_tree:
            annotations.append(AnnotateCode(
                row=max(int(error_element.attrib['line']) - 1, 0),
                text=error_element.attrib['message'],
                column=max(int(error_element.attrib['column']) - 1, 0),
                type=message_categories[error_element.attrib.get('severity',
                                                                 Severity.WARNING)],
            ))

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
