"""
Main file, responsible for running TESTed based on the input given by Dodona.
"""
from typing import IO
from os.path import splitext

from .configs import DodonaConfig, create_bundle
from .testplan import parse_test_plan
from .dsl import SchemaParser


def run(config: DodonaConfig, judge_output: IO):
    """
    Run the TESTed judge.

    :param config: The configuration, as received from Dodona.
    :param judge_output: Where the judge output will be written to.
    """
    with open(f"{config.resources}/{config.testplan}", "r") as t:
        textual_plan = t.read()

    _, ext = splitext(config.testplan)
    is_yaml = ext.lower() in (".yaml", ".yml")
    if is_yaml:
        parser = SchemaParser()
        plan = parser.load_str(textual_plan)
    else:
        plan = parse_test_plan(textual_plan)
    pack = create_bundle(config, judge_output, plan)

    from .judge import judge
    judge(pack)
