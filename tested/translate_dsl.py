from argparse import ArgumentParser, FileType
from tested.dsl import SchemaParser
from tested.utils import smart_close

if __name__ == "__main__":
    parser = ArgumentParser(
        description="The universal judge for Dodona.",
        usage='%(prog)s [-h] [-i DSL] [-o JSON] [dsl [json]]'
    )
    parser.add_argument('-i', '--dsl', type=FileType('r'),
                        help="Where the test plan should be read from.",
                        default="-")
    parser.add_argument('-o', '--json', type=FileType('w'),
                        help="Where the translate test plan should be written to.",
                        default="-")
    parser.add_argument('dsl_file', metavar='dsl', nargs='?', type=FileType('r'),
                        help="Where the test plan should be read from, override "
                             "option when given.")
    parser.add_argument('json_file', metavar='json', nargs='?', type=FileType('w'),
                        help="Where the translate test plan should be written to, "
                             "override option when given.")

    parser = parser.parse_args()
    schema_parser = SchemaParser()
    if parser.dsl_file is not None:
        smart_close(parser.dsl)
        parser.dsl = parser.dsl_file
        if parser.json_file is not None:
            smart_close(parser.json)
            parser.json = parser.json_file

    with smart_close(parser.dsl) as test_plan:
        yaml_str = test_plan.read()
    json_str = schema_parser.translate_str(yaml_str=yaml_str)

    with smart_close(parser.json) as json_test_plan:
        print(json_str, file=json_test_plan)
