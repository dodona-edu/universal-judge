#
# Run things manually.
#
import json
import sys
from typing import NamedTuple

from tested import Config
from testplan import parse_test_plan


LANGUAGE_TO_KERNEL = {
    'python': 'python3',
    'python3': 'python3',
    'r': 'ir',
    'julia': 'julia-1.2',
    'haskell': 'ihaskell',
    'javascript': 'javascript'
}


def read_config() -> Config:
    """Read the configuration from stdout"""
    config_ = {
        "memory_limit": 536870912,
        "time_limit": 10000000,
        "programming_language": 'python',
        "natural_language": 'nl',
        "resources": '../exercise',
        "source": './tests/python/deliberate_stop.py',
        "judge": '../',
        "workdir": './workdir',
    }
    needed_config = {x: config_[x] for x in Config._fields if x in config_}
    needed_config['kernel'] = LANGUAGE_TO_KERNEL.get(config_["programming_language"], config_["programming_language"])
    return Config(**needed_config)


if __name__ == '__main__':
    config = read_config()

    # For now, the action plano is as follows:
    # 1. Load the student user_code in the kernel
    # 2. Process the input line-by-line
    #    - Check the output for each element.
    # 3. Finish

    json_string = open(f"{config.resources}/basic.json").read()
    plan = parse_test_plan(json_string)

    # Run it.
    from judge import GeneratorJudge
    tester = GeneratorJudge(config)
    tester.judge(plan)
