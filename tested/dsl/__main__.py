from argparse import ArgumentParser

from tested.dsl.translate_parser import translate_to_test_suite
from tested.utils import smart_open

parser = ArgumentParser(description="Convert a DSL test suite to the JSON test suite.")
parser.add_argument(
    "-i",
    "--input",
    help="The DSL file (in YAML)",
    default="-",
)
parser.add_argument(
    "-o",
    "--output",
    help="The location for the JSON test suite",
    default="-",
)

parser = parser.parse_args()

with smart_open(parser.input, "r") as input_file:
    dsl = input_file.read()

suite = translate_to_test_suite(dsl)

with smart_open(parser.output, "w") as output_file:
    output_file.write(suite)
