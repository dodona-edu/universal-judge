import logging
import sys
from argparse import ArgumentParser, FileType

from tested.configs import read_config
from tested.main import run
from tested.utils import smart_close

parser = ArgumentParser(
    description="The programming-language-agnostic educational test framework."
)
parser.add_argument(
    "-c",
    "--config",
    type=FileType("r"),
    help="Where to read the config from",
    default="-",
)
parser.add_argument(
    "-o",
    "--output",
    type=FileType("w"),
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

parser.add_argument(
    "-t",
    "--translate",
    type=str,
    help="Specifies the language to translate translate the dsl to.",
    default='-'
)
parser = parser.parse_args()

if parser.verbose:
    log = logging.getLogger()
    log.setLevel(logging.DEBUG)
    ch = logging.StreamHandler(stream=sys.stdout)
    formatter = logging.Formatter("%(name)s:%(levelname)s:%(message)s")
    ch.setFormatter(formatter)
    log.addHandler(ch)

configuration = read_config(parser.config)
with smart_close(parser.output) as out:
    run(configuration, out, parser.translate)
