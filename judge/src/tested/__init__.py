"""
The TESTed package is a universal judge for the Dodona platform.
More information can be found in the thesis manuscript.

If you are interested in running TESTed, you can:

1. Use this package as an executable module (python -m tested). This will use the
   command line interface, which is also available to Dodona.
2. Run it programmatically, by running the manual module (python -m tested.manual).
   This should allow easier usage and illustrates programmatic use.
"""
if __name__ == '__main__':
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
