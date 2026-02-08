import logging
from pathlib import Path
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
    "info": Severity.INFO,
    "ignore": Severity.INFO,
}


def run_checkstyle(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls checkstyle to annotate submitted source code and adds resulting score and
    annotations to tab.
    """
    submission = config.source
    language_options = config.config_for()

    if path := language_options.get("checkstyle_config", None):
        assert isinstance(path, str)
        config_path = config.resources / path
    else:
        # Use the default file.
        config_path = config.judge / "tested/languages/java/sun_tested_checks.xml"
    config_path = str(config_path.absolute())

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=["checkstyle", "-f", "xml", "-c", config_path, submission.name],
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            (
                get_i18n_string("languages.java.linter.timeout")
                if execution_results.timeout
                else get_i18n_string("languages.java.linter.memory")
            )
        ], []

    try:
        xml_tree = ElementTree.fromstring(execution_results.stdout)
    except Exception as e:
        logger.warning("Checkstyle produced bad output", exc_info=e)
        return [
            get_i18n_string("languages.java.linter.output"),
            ExtendedMessage(
                description=str(e), format="code", permission=Permission.STAFF
            ),
        ], []
    annotations = []

    for file_tree in xml_tree:
        if Path(file_tree.attrib.get("name", submission)).name != submission.name:
            continue
        for error_element in file_tree:
            message = error_element.attrib.get("message", None)
            if not message:
                continue
            source = error_element.attrib.get("source", None)
            external = None
            if source:
                external = f'https://checkstyle.sourceforge.io/apidocs/index.html?{source.replace(".", "/")}.html'

            position = get_linter_position(
                raw_start_row=error_element.attrib.get("line"),
                source_offset=config.source_offset,
                raw_end_row=None,
                raw_start_column=error_element.attrib.get("column"),
                raw_end_column=None,
            )

            annotations.append(
                annotation_from_position(
                    position=position,
                    text=message,
                    external_url=external,
                    type=message_categories.get(
                        error_element.attrib.get("severity", "warning"),
                        Severity.WARNING,
                    ),
                )
            )

    return [], annotations
