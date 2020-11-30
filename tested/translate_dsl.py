from argparse import ArgumentParser, FileType
from tested.dsl import translate
from tested.utils import smart_close

if __name__ == "__main__":
    parser = ArgumentParser(
        description="The universal judge for Dodona."
    )
    parser.add_argument('-i', '--testplan', type=FileType('r'),
                        help="Where the test plan should be read from.", default="-")
    parser.add_argument('-o', '--output', type=FileType('w'),
                        help="Where the translate test plan should be written to.",
                        default="-")
    parser = parser.parse_args()

    with smart_close(parser.testplan) as test_plan:
        yaml_str = test_plan.read()

    json_str = translate(yaml_str=yaml_str)

    with smart_close(parser.output) as json_test_plan:
        print(json_str, file=json_test_plan)
