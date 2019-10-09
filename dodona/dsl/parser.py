# DSL types.
#
# Eventually, the DSL should look a bit like this:
#
# with Judgement() as judgement:
#
# end
import json
from dataclasses import dataclass, field
from dataclasses_json import dataclass_json
from typing import List, Optional


@dataclass_json
@dataclass
class Stdin:
    data: Optional[str] = None
    type: Optional[str] = "text"


@dataclass_json
@dataclass
class Stdout:
    type: Optional[str] = "text"
    data: Optional[str] = None
    ignore: bool = False


@dataclass_json
@dataclass
class FileOutput:
    expected: str
    actual: str


@dataclass_json
@dataclass
class Input:
    stdin: Optional[Stdin] = Stdin()
    # Add function calls and such here.


@dataclass_json
@dataclass
class Output:
    # Receive output on stdout
    stdout: Optional[Stdout] = Stdout()
    # The output should be a file.
    file: Optional[FileOutput] = None


@dataclass_json
@dataclass
class Evaluator:
    name: str
    type: Optional[str] = "builtin"
    language: Optional[str] = None
    options: Optional[List[str]] = field(default_factory=list)


@dataclass_json
@dataclass
class Test:
    description: str
    input: Input
    output: Output
    evaluator: Optional[Evaluator] = Evaluator(name="comparator")


@dataclass_json
@dataclass
class Testcase:
    description: str
    tests: List[Test]


@dataclass_json
@dataclass
class Context:
    description: str
    testcases: List[Testcase]


@dataclass_json
@dataclass
class Tab:
    name: str
    contexts: List[Context]
    description: Optional[str] = None


@dataclass_json
@dataclass
class Plan:
    tabs: List[Tab]


def parse_test_plan(file) -> Plan:
    """Parse a test plan into the structures."""

    # Load json
    with open(file, "r") as file:
        raw = file.read()

    # We assume it is valid
    return Plan.from_json(raw)


if __name__ == '__main__':
    parse_test_plan("./internal.json")
