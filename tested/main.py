"""
Main file, responsible for running TESTed based on the input given by Dodona.
"""
from typing import IO
from os.path import splitext

from .configs import DodonaConfig, create_bundle
from .testsuite import parse_test_suite
from .dsl import parse_dsl

from . import internal_timings


def run(config: DodonaConfig, judge_output: IO):
    """
    Run the TESTed judge.

    :param config: The configuration, as received from Dodona.
    :param judge_output: Where the judge output will be written to.
    """
    internal_timings.collect_timings(config.timing_statistics)

    internal_timings.new_stage("test_suite")
    with open(f"{config.resources}/{config.test_suite}", "r") as t:
        textual_suite = t.read()

    _, ext = splitext(config.test_suite)
    is_yaml = ext.lower() in (".yaml", ".yml")
    if is_yaml:
        internal_timings.new_stage("dsl")
        suite = parse_dsl(textual_suite)
        internal_timings.end_stage("dsl")
    else:
        internal_timings.new_stage("json")
        suite = parse_test_suite(textual_suite)
        internal_timings.end_stage("json")
    internal_timings.new_stage("bundle")
    pack = create_bundle(config, judge_output, suite)
    internal_timings.end_stage("bundle")
    from .judge import judge

    judge(pack)
