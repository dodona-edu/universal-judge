from argparse import ArgumentParser, FileType
from .translate_parser import translate_to_test_suite
from ..utils import smart_close

parser = ArgumentParser(description="Convert a DSL test suite to the JSON test suite.")
parser.add_argument(
    "-i",
    "--input",
    type=FileType("r"),
    help="The DSL file (in YAML)",
    default="-",
)
parser.add_argument(
    "-o",
    "--output",
    type=FileType("w"),
    help="The location for the JSON test suite",
    default="-",
)

parser = parser.parse_args()

with smart_close(parser.input) as input_file:
    dsl = input_file.read()

suite = translate_to_test_suite(dsl, validate=True)

with smart_close(parser.output) as output_file:
    output_file.write(suite)
