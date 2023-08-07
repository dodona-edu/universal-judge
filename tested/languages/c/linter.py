import logging
from typing import List, Tuple
from xml.etree import ElementTree

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)

message_categories = {
    "error": Severity.ERROR,
    "warning": Severity.WARNING,
    "style": Severity.INFO,
    "performance": Severity.INFO,
    "portability": Severity.INFO,
    "information": Severity.INFO,
}


def run_cppcheck(
    config: DodonaConfig, remaining: float, language: str = "c"
) -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls cppcheck to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source
    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=[
            "cppcheck",
            "--xml",
            "--enable=style,warning",
            f"--language={language}",
            submission.name,
        ],
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            get_i18n_string("languages.c.linter.timeout")
            if execution_results.timeout
            else get_i18n_string("languages.c.linter.memory")
        ], []

    try:
        xml_tree = ElementTree.fromstring(execution_results.stderr)
    except Exception as e:
        logger.warning("cppcheck produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.c.linter.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
        ], []
    annotations = []

    for element in xml_tree:
        if element.tag != "errors":
            continue
        for error in element:
            message = error.attrib.get("verbose", None)
            if not message:
                continue
            severity = error.attrib.get("severity", "warning")
            row = None
            col = None
            for el in error:
                if el.tag != "location":
                    continue
                row = int(el.attrib.get("line", "1")) - 1 + config.source_offset
                col = int(el.attrib.get("column", "1")) - 1
                break
            annotations.append(
                AnnotateCode(
                    row=row or 0,
                    text=message,
                    column=col,
                    type=message_categories.get(severity, Severity.WARNING),
                )
            )

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    return [], annotations
