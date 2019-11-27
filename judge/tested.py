#
# Main file of the TESTed framework.
#
# For now, this only handles running the execution kernel.
#
import dataclasses
import json
import sys
from dataclasses import dataclass

from testplan import parse_test_plan


@dataclass
class Config:
    resources: str
    source: str
    time_limit: str
    memory_limit: str
    natural_language: str
    programming_language: str
    workdir: str
    judge: str


def read_config() -> Config:
    """Read the configuration from stdout"""
    config_json = sys.stdin.read()
    config_ = json.loads(config_json)
    required = [x.name for x in dataclasses.fields(Config)]
    needed_config = {x: config_[x] for x in required if x in config_}
    return Config(**needed_config)


if __name__ == '__main__':
    config = read_config()

    # Read test plan
    json_string = open(f"{config.resources}/plan.json").read()
    plan = parse_test_plan(json_string)

    # Run it.
    from judge import GeneratorJudge

    tester = GeneratorJudge(config)
    tester.judge(plan)
