"""
Main file, responsible for running TESTed based on the input given by Dodona.
"""
from typing import IO
from os.path import splitext

from .configs import DodonaConfig, create_bundle
from .testplan import parse_test_plan
from .dsl import SchemaParser

from . import internal_timings


def run(config: DodonaConfig, judge_output: IO):
    """
    Run the TESTed judge.

    :param config: The configuration, as received from Dodona.
    :param judge_output: Where the judge output will be written to.
    """
    internal_timings.collect_timings(config.timing_statistics)

    internal_timings.new_stage("testplan")
    with open(f"{config.resources}/{config.testplan}", "r") as t:
        textual_plan = t.read()

    _, ext = splitext(config.testplan)
    is_yaml = ext.lower() in (".yaml", ".yml")
    if is_yaml:
        internal_timings.new_stage("dsl")
        parser = SchemaParser()
        plan = parser.load_str(textual_plan)
    else:
        internal_timings.new_stage("json")
        plan = parse_test_plan(textual_plan)
    internal_timings.new_stage("bundle")
    pack = create_bundle(config, judge_output, plan)
    internal_timings.end_stage("bundle")
    from .judge import judge

    judge(pack)
