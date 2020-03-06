import dataclasses
import json
import os
import shutil
from dataclasses import dataclass, field

from argparse import ArgumentParser, FileType
from typing import IO, Dict, Optional

from testplan import parse_test_plan
from utils import smart_close


@dataclass(frozen=True)
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
    linter: Optional[bool] = None
    plan_name: str = "plan.json"  # Name of the testplan file.
    options: Dict[str, str] = field(default_factory=dict)


def read_config(config_in: IO) -> Config:
    """
    Read the configuration from the given file. If the file is not stdin, it will be
    closed.
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

    # Merge config from config into testplan if needed.
    if config.linter is not None:
        existing = testplan.language_config(config.programming_language)
        existing["linter"] = config.linter
        testplan.configuration.language[config.programming_language] = existing

    from judge import GeneratorJudge
    tester = GeneratorJudge(config, judge_output)
    tester.judge(testplan)


def clean_working_directory(config: Config):
    """
    Delete everything in the working directory.
    """
    for root, dirs, files in os.walk(config.workdir):
        for f in files:
            os.unlink(os.path.join(root, f))
        for d in dirs:
            shutil.rmtree(os.path.join(root, d), ignore_errors=True)


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
