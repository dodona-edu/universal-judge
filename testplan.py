from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional

from dataclasses_json import dataclass_json


class TestPlanError(ValueError):
    pass


# Special class denoting the name of the object that must be run.
@dataclass_json
@dataclass
class Run:
    object: str


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
    name: Optional[str] = "textComparator"
    type: Optional[str] = "builtin"
    language: Optional[str] = None
    options: Optional[List[str]] = field(default_factory=list)


@dataclass_json
@dataclass
class Test:
    description: str
    input: Input
    output: Output
    evaluator: Optional[Evaluator] = None
    run: Optional[Run] = None

    def get_run(self):
        return self.run if self.run else self.parent.get_run()


@dataclass_json
@dataclass
class Testcase:
    description: str
    tests: List[Test]
    run: Optional[Run] = None

    def get_run(self):
        return self.run if self.run else self.parent.get_run()


@dataclass_json
@dataclass
class Context:
    testcases: List[Testcase]
    description: Optional[str] = None
    run: Optional[Run] = None

    def get_run(self):
        return self.run if self.run else self.parent.get_run()


@dataclass_json
@dataclass
class Tab:
    name: str
    contexts: List[Context]
    description: Optional[str] = None
    run: Optional[Run] = None

    def get_run(self):
        return self.run if self.run else self.parent.get_run()


@dataclass_json
@dataclass
class Plan:
    name: Optional[str] = None
    tabs: Optional[List[Tab]] = field(default_factory=list)
    run: Optional[Run] = None

    def get_run(self):
        return self.run if self.run else self.parent.get_run()


def parse_test_plan(test_file) -> Plan:
    """Parse a test plan into the structures."""

    raw = test_file.read()
    plan: Plan = Plan.from_json(raw)

    if not plan.name:
        plan.name = Path(test_file.name).stem

    # Add parent to all elements
    for tab in plan.tabs:
        tab.parent = plan
        for context in tab.contexts:
            context.parent = tab
            for testcase in context.testcases:
                testcase.parent = context
                for test in testcase.tests:
                    test.parent = testcase

    return plan


if __name__ == '__main__':
    with open('dsl/internal.json', 'r') as f:
        r = parse_test_plan(f)
