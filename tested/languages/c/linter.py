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
    'error':       Severity.ERROR,
    'warning':     Severity.WARNING,
    'style':       Severity.INFO,
    'performance': Severity.INFO,
    'portability': Severity.INFO,
    'information': Severity.INFO,
}


def run_cppcheck(bundle: Bundle, submission: Path, remaining: float,
                 language: str = 'c') -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls cppcheck to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    config = bundle.config
    language_options = bundle.config.config_for()

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=["cppcheck", "--xml", "--enable=style,warning",
                 f"--language={language}", submission.name]
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [get_i18n_string(
            "languages.c.linter.timeout") if execution_results.timeout else
                get_i18n_string("languages.c.linter.memory")], []

    try:
        xml_tree = ElementTree.fromstring(execution_results.stderr)
    except Exception as e:
        logger.warning("cppcheck produced bad output", exc_info=e)
        return [get_i18n_string("languages.c.linter.output"),
                ExtendedMessage(
                    description=str(e),
                    format='code',
                    permission=Permission.STAFF
                )], []
    annotations = []

    for element in xml_tree:
        if element.tag != 'errors':
            continue
        for error in element:
            message = error.attrib["verbose"]
            severity = error.attrib["severity"]
            position = None
            for el in error:
                if el.tag != 'location':
                    continue
                position = (max(int(el.attrib["line"]) - 1, 0),
                            max(int(el.attrib["column"]) - 1, 0))
                break
            annotations.append(AnnotateCode(
                row=position[0],
                text=message,
                column=position[1],
                type=message_categories.get(severity, Severity.WARNING)
            ))

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
