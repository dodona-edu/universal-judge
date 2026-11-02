import logging
import sys
from argparse import ArgumentParser

from tested.configs import read_config
from tested.main import run
from tested.utils import smart_open

parser = ArgumentParser(
    description="The programming-language-agnostic educational test framework."
)
parser.add_argument(
    "-c",
    "--config",
    help="Where to read the config from",
    default="-",
)
parser.add_argument(
    "-o",
    "--output",
    help="Where the judge output should be written to.",
    default="-",
)
parser.add_argument(
    "-v",
    "--verbose",
    dest="verbose",
    help="Include verbose logs. It is recommended to also use -o in this case.",
    action="store_true",
)
parser = parser.parse_args()

if parser.verbose:
    log = logging.getLogger()
    log.setLevel(logging.DEBUG)
    ch = logging.StreamHandler(stream=sys.stdout)
    formatter = logging.Formatter("%(name)s:%(levelname)s:%(message)s")
    ch.setFormatter(formatter)
    log.addHandler(ch)

with smart_open(parser.config, "r") as config_in:
    configuration = read_config(config_in)
with smart_open(parser.output, "w") as out:
    run(configuration, out)
