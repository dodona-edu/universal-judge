from argparse import ArgumentParser, FileType
from .configs import read_config
from .utils import smart_close
from .main import run

parser = ArgumentParser(
    description="The universal judge for Dodona."
)
parser.add_argument('-p', '--testplan', type=FileType('r'),
                    help="Where to read the configs from", default="-")
parser.add_argument('-o', '--output', type=FileType('w'),
                    help="Where the judge output should be written to.",
                    default="-")
parser = parser.parse_args()

configuration = read_config(parser.testplan)
with smart_close(parser.output) as out:
    run(configuration, out)
