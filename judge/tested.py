#
# Main file of the TESTed framework.
#
# For now, this only handles running the main kernel.
#
import dataclasses
import json
from dataclasses import dataclass

from argparse import ArgumentParser, FileType
from typing import IO

from testplan import parse_test_plan
from utils import smart_close


@dataclass
class Config:
    resources: str
    source: str
    time_limit: str
    memory_limit: str
    natural_language: str
    programming_language: str
    # noinspection SpellCheckingInspection
    workdir: str
    judge: str
    plan_name: str = "plan.json"  # Name of the testplan file.


def read_config(config_in: IO) -> Config:
    """
    Read the configuration from the given file. If the file is not stdin, it will be closed.
    """
    with smart_close(config_in) as input_:
        config_json = input_.read()
    config_ = json.loads(config_json)
    required = [x.name for x in dataclasses.fields(Config)]
    needed_config = {x: config_[x] for x in required if x in config_}
    return Config(**needed_config)


def run(config: Config, judge_output: IO):
    """
    Run the universal judge.
    :param config: The configuration, as received from Dodona.
    :param judge_output: Where the judge output will be written to.
    """
    with open(f"{config.resources}/{config.plan_name}", "r") as t:
        textual_plan = t.read()

    testplan = parse_test_plan(textual_plan)
    from judge import GeneratorJudge
    tester = GeneratorJudge(config, judge_output)
    tester.judge(testplan)


if __name__ == '__main__':
    parser = ArgumentParser(
        description="The universal judge for Dodona."
    )
    parser.add_argument('-p', '--testplan', type=FileType('r'),
                        help="Where to read the config from", default="-")
    parser.add_argument('-o', '--output', type=FileType('w'),
                        help="Where the judge output should be written to.", default="-")
    parser = parser.parse_args()

    configuration = read_config(parser.testplan)
    with smart_close(parser.output) as out:
        run(configuration, out)
