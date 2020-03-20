import os
import shutil

from argparse import ArgumentParser, FileType
from typing import IO

from tested import Config, Bundle
from tested.config import Config, read_config, Bundle, create_bundle

from .languages import get_language_config
from .testplan import parse_test_plan
from .utils import smart_close, _get_identifier


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
        existing = testplan.config_for(config.programming_language)
        existing["linter"] = config.linter
        testplan.configuration.language[config.programming_language] = existing

    from judge.__init__ import judge
    pack = create_bundle(config, judge_output, testplan)
    judge(pack)


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
