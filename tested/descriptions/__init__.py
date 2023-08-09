import os
from pathlib import Path

from tested.configs import Bundle, DodonaConfig, GlobalConfig
from tested.descriptions.converters import (
    convert_templated_problem,
    convert_tested_markdown,
)
from tested.languages import get_language
from tested.testsuite import Suite


def process_problem_statement(
    problem_statement: str, programming_language: str, natural_language: str = "en"
) -> str:
    judge_directory = Path(__file__).parent.parent
    global_config = GlobalConfig(
        dodona=DodonaConfig(
            resources=Path(),
            source=Path(),
            time_limit=0,
            memory_limit=0,
            natural_language=natural_language,
            programming_language=programming_language,
            workdir=Path(),
            judge=judge_directory,
        ),
        context_separator_secret="",
        testcase_separator_secret="",
        suite=Suite(tabs=[]),
    )

    language = get_language(global_config, global_config.dodona.programming_language)

    bundle = Bundle(
        global_config=global_config,
        lang_config=language,
        out=open(os.devnull, "w"),
    )

    tested_markdown = convert_templated_problem(bundle, problem_statement)
    normal_markdown = convert_tested_markdown(bundle, tested_markdown)

    if not tested_markdown.endswith("\n"):
        normal_markdown = normal_markdown.rstrip("\n")

    return normal_markdown
