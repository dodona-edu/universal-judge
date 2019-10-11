#
# Main file of the TESTed framework.
#
# For now, this only handles running the execution kernel.
#
import json
import sys
from typing import NamedTuple


class Config(NamedTuple):
    resources: str
    source: str
    time_limit: str
    memory_limit: str
    natural_language: str
    programming_language: str
    workdir: str
    judge: str
    kernel: str


LANGUAGE_TO_KERNEL = {
    'python': 'python3',
    'python3': 'python3',
    'r': 'ir',
    'julia': 'julia-1.2',
    'haskell': 'ihaskell'
}


def read_config() -> Config:
    """Read the configuration from stdout"""
    config_ = {
        "memory_limit": 536870912,
        "time_limit": 10000000,
        "programming_language": 'java',
        "natural_language": 'nl',
        "resources": './excercise',
        "source": './excercise/test.java',
        "judge": 'ignored',
        "workdir": 'ignored',
    }
    #config_json = sys.stdin.read()
    #config_ = json.loads(config_json)
    needed_config = {x: config_[x] for x in Config._fields if x in config_}
    needed_config['kernel'] = LANGUAGE_TO_KERNEL.get(config_["programming_language"], config_["programming_language"])
    return Config(**needed_config)


if __name__ == '__main__':
    config = read_config()

    # For now, the action plan is as follows:
    # 1. Load the student user_code in the kernel
    # 2. Process the input line-by-line
    #    - Check the output for each element.
    # 3. Finish

    # Read user_code
    with open(config.source, 'r') as f:
        user_code = f.read()

    # Run it.
    from judge import KernelJudge
    tester = KernelJudge(config)
    tester.judge()
