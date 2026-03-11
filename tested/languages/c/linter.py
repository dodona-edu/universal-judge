import logging
from xml.etree import ElementTree

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.linter import annotation_from_position, get_linter_position
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
) -> tuple[list[Message], list[AnnotateCode]]:
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
            (
                get_i18n_string("languages.c.linter.timeout")
                if execution_results.timeout
                else get_i18n_string("languages.c.linter.memory")
            )
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

            location_element = error.find("location")
            if location_element is not None:
                position = get_linter_position(
                    raw_start_row=location_element.attrib.get("line"),
                    source_offset=config.source_offset,
                    raw_end_row=None,
                    raw_start_column=location_element.attrib.get("column"),
                    raw_end_column=None,
                )
            else:
                position = get_linter_position(
                    raw_start_row=None,
                    source_offset=config.source_offset,
                    raw_end_row=None,
                    raw_start_column=None,
                    raw_end_column=None,
                )

            annotations.append(
                annotation_from_position(
                    position=position,
                    text=message,
                    type=message_categories.get(severity, Severity.WARNING),
                )
            )

    return [], annotations
