import json
import logging

from tested.configs import DodonaConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission, Severity
from tested.internationalization import get_i18n_string
from tested.judge.utils import run_command

logger = logging.getLogger(__name__)

message_categories = {
    "p1": Severity.ERROR,
    "p2": Severity.WARNING,
    "p3": Severity.INFO,
}

def run_codenarc(
    config: DodonaConfig, remaining: float
) -> tuple[list[Message], list[AnnotateCode]]:
    """
    Calls CodeNarc with linter rules for Nextflow to annotate submitted source code
    and adds resulting score and annotations to tab.
    """
    submission = config.source
    language_options = config.config_for()

    # TODO: find a better solution
    if path := language_options.get("codenarc_path", None):
        assert isinstance(path, str)
        codenarc_path = config.resources / path
    else:
        return [], []
    codenarc_path = str(codenarc_path.absolute())
    report_file = str((config.workdir / "report.json").absolute())

    execution_results = run_command(
        directory=submission.parent,
        timeout=remaining,
        command=[
            "java",
            "-Dorg.slf4j.simpleLogger.defaultLogLevel=error",
            "-classpath",
            f"{codenarc_path}/linter-rules-0.1.jar:{codenarc_path}/CodeNarc-3.5.0-all.jar:{codenarc_path}/slf4j-api-1.7.36.jar:{codenarc_path}/slf4j-simple-1.7.36.jar",
            "org.codenarc.CodeNarc",
            f"-report=json:{report_file}",
            "-rulesetfiles=rulesets/general.xml",
            "-includes=**/*.nf",
            f"-includes={submission.name}"]
    )

    if execution_results is None:
        return [], []

    if execution_results.timeout or execution_results.memory:
        return [
            (
                get_i18n_string("languages.nextflow.linter.timeout")
                if execution_results.timeout
                else get_i18n_string("languages.nextflow.linter.memory")
            )
        ], []

    with open(report_file) as json_file:
        try:
            result = json.load(json_file)
        except Exception as e:
            logger.warning("CodeNarc produced bad output", exc_info=e)
            return [
                get_i18n_string("languages.nextflow.linter.output"),
                ExtendedMessage(
                    description=str(e), format="code", permission=Permission.STAFF
                ),
            ], []
        annotations = []

        for package in result.get("packages"):
            for file in package.get("files"):
                if file.get("name") != submission.name:
                    continue
                for violation in file.get("violations"):
                    message = violation.get("message")
                    if not message:
                        continue
                    annotations.append(
                        AnnotateCode(
                            row=violation.get("line_number", 0) + config.source_offset,
                            text=message,
                            externalUrl=None,
                            column=0,
                            type=message_categories.get(
                                violation.get("priority"),
                                Severity.WARNING,
                            ),
                        )
                    )

        # sort linting messages on line, column and code
        annotations.sort(key=lambda a: (a.row, a.column, a.text))
        return [], annotations
