"""
Script to convert a "generic" Markdown problem statement into a language-specific
problem statement.

A generic problem statement has two special aspects:

1. The problem statement is a Mako template, so conditionals are possible.
   See the section below for information on some available variables.
2. Code blocks in Markdown that are annotated with the language `tested` will
   be converted into the programming language.


== Mako templates ==

The following variables are available:

- `programming_language`: name of the programming language (e.g. `java`)

To convert a name of something (e.g. a class) to the conventions of the specific
programming language, the following functions are available: `namespace`, `function`,
`identifier`, `property`, `clazz`, `global_identifier`.

Finally, there are two functions that convert "TESTed" data types to either the type
in the programming language (e.g. "sequence" becomes "list" in Python"). You can use
the function `datatype` for this.

== Code blocks ==

All code blocks that are marked as `tested` will be treated as DSL expressions and
statements. They can thus use the Python-like syntax. See the docs on the DSL for
more information.

== Other ==

Note that the file is first converted by Mako to a normal Markdown file.
Afterwards, the code blocks are replaced.
"""
import sys
from argparse import ArgumentParser, FileType

from tested.descriptions import process_problem_statement
from tested.languages import LANGUAGES
from tested.utils import smart_close

parser = ArgumentParser(
    description="Convert generic problem statements to language-specific ones"
)

parser.add_argument(
    "-p",
    "--problem",
    type=FileType("r"),
    help="Generic problem statement",
    default="-",
)
parser.add_argument(
    "-o",
    "--output",
    type=FileType("w"),
    help="Language-specific problem statement",
    default="-",
)
parser.add_argument(
    "-l",
    "--language",
    type=str,
    help="Programming language to convert the generic problem statement into.",
    choices=LANGUAGES.keys(),
)
parser.add_argument(
    "-n",
    "--natural_language",
    type=str,
    help="The natural language for the names of data types.",
    default="en",
)

parser = parser.parse_args()

try:
    with smart_close(parser.problem) as problem_fd:
        problem_statement = problem_fd.read()

    result = process_problem_statement(
        problem_statement, parser.language, parser.natural_language
    )

    with smart_close(parser.output) as output_fd:
        print(result, file=output_fd)

except ValueError as e:
    print(e, file=sys.stderr)
    sys.exit(-1)
