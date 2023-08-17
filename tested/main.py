"""
Main file, responsible for running TESTed based on the input given by Dodona.
"""
import os
from typing import IO

from tested.configs import DodonaConfig, create_bundle
from tested.dsl import parse_dsl
from tested.testsuite import parse_test_suite


def run(config: DodonaConfig, judge_output: IO):
    """
    Run the TESTed judge.

    :param config: The configuration, as received from Dodona.
    :param judge_output: Where the judge output will be written to.
    """
    try:
        with open(f"{config.resources}/{config.test_suite}", "r") as t:
            textual_suite = t.read()
    except FileNotFoundError as e:
        print("The test suite was not found. Check your exercise's config.json file.")
        print("Remember that the test suite is a path relative to the 'evaluation' folder of your exercise.")
        raise e

    _, ext = os.path.splitext(config.test_suite)
    is_yaml = ext.lower() in (".yaml", ".yml")
    if is_yaml:
        suite = parse_dsl(textual_suite)
    else:
        suite = parse_test_suite(textual_suite)
    pack = create_bundle(config, judge_output, suite)
    from .judge import judge

    judge(pack)
