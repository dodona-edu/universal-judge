from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import List, Optional, Union
from dataclasses_json import dataclass_json


class TestPlanError(ValueError):
    """Error when the test plan is not valid."""
    pass


# Special class denoting the name of the object that must be run.
@dataclass_json
@dataclass
class Run:
    classname: str


class DataType(Enum):
    text = "text"  # Literal values
    file = "file"  # Path to a file


class ChannelState(Enum):
    none = "none"  # There is nothing on this channel, i.e. completely empty.


class OutputChannelState(Enum):
    none = "none"  # There is nothing on this channel, i.e. completely empty.
    ignored = "ignored"  # Ignore everything on this channel, i.e. doesn't matter what you use.


@dataclass_json
@dataclass
class ChannelData:
    """Describes the data on channel (stdin or stdout)"""
    data: str  # The actual data.
    type: Optional[DataType] = DataType.text  # The type of the data field.


InputChannel = Union[ChannelData, ChannelState]
OutputChannel = Union[ChannelData, OutputChannelState]


@dataclass_json
@dataclass
class FileOutput:
    """Describes output where the code should produce a file."""
    expected: str  # Path to the expected file
    actual: str  # Path to the generated file (by the users code)


@dataclass_json
@dataclass
class Input:
    """Describes the input channels for a test"""
    stdin: Optional[InputChannel] = ChannelState.none  # Describes stdin. If "none", there is no stdin.
    # Add function calls and such here, as separate input channels.


@dataclass_json
@dataclass
class Output:
    """Describes the output channels for a test"""
    stdout: Optional[OutputChannel] = OutputChannelState.ignored  # Stdout is ignored by default
    stderr: Optional[OutputChannel] = OutputChannelState.none  # Stderr should be empty by default
    file: Optional[FileOutput] = None  # The output can be a file.


class EvaluatorType(Enum):
    builtin = "builtin"
    external = "external"


TEXT_COMPARATOR = "textComparator"
TEXT_ARGUMENTS = ["ignoreWhitespace"]


@dataclass_json
@dataclass
class Evaluator:
    """Will evaluate the test."""
    name: Optional[str] = TEXT_COMPARATOR  # Name of the comparator or path to the evaluator.
    type: Optional[EvaluatorType] = EvaluatorType.builtin  # Type of the evaluator.
    language: Optional[str] = None  # Ignored when using built-in evaluators
    options: Optional[List[str]] = field(default_factory=lambda: TEXT_ARGUMENTS)  # Pass options to the evaluator.


@dataclass_json
@dataclass
class Test:
    """Describes a single test"""
    description: str  # Description of the test.
    input: Input  # Input of the test.
    output: Output  # Output of the test.
    evaluator: Optional[Evaluator] = None
    runArgs: Optional[Run] = None  # Optional run arguments TODO

    def get_run(self):
        return self.run if self.run else self.parent.get_run()

    def get_evaluator(self):
        return self.evaluator if self.evaluator else self.parent.get_evaluator()


@dataclass_json
@dataclass
class Testcase:
    """Describes a test case, consisting of some tests."""
    description: str  # Description of the test case.
    tests: List[Test]  # Tests in the test case.
    run: Optional[Run] = None  # Optional run arguments
    evaluator: Optional[Evaluator] = None

    def get_run(self):
        return self.run if self.run else self.parent.get_run()

    def get_evaluator(self):
        return self.evaluator if self.evaluator else self.parent.get_evaluator()


@dataclass_json
@dataclass
class Context:
    testcases: List[Testcase]
    description: Optional[str] = None
    run: Optional[Run] = None
    evaluator: Optional[Evaluator] = None

    def get_run(self):
        return self.run if self.run else self.parent.get_run()

    def get_evaluator(self):
        return self.evaluator if self.evaluator else self.parent.get_evaluator()


@dataclass_json
@dataclass
class Tab:
    name: str
    contexts: List[Context]
    description: Optional[str] = None
    run: Optional[Run] = None
    evaluator: Optional[Evaluator] = None

    def get_run(self):
        return self.run if self.run else self.parent.get_run()

    def get_evaluator(self):
        return self.evaluator if self.evaluator else self.parent.get_evaluator()


@dataclass_json
@dataclass
class Plan:
    name: Optional[str] = None
    tabs: Optional[List[Tab]] = field(default_factory=list)
    run: Optional[Run] = None
    evaluator: Optional[Evaluator] = Evaluator()

    def get_run(self):
        return self.run

    def get_evaluator(self):
        return self.evaluator


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


def parse_test_plan_json(json_string) -> Plan:
    """Parse a test plan into the structures."""

    plan: Plan = Plan.from_json(json_string)
    plan.name = "Deprecated, will be removed."

    # TODO: properly fix the union types somehow
    for tab in plan.tabs:
        for context in tab.contexts:
            for testcase in context.testcases:
                for test in testcase.tests:
                    if isinstance(test.input.stdin, dict):
                        test.input.stdin = ChannelData.from_dict(test.input.stdin)
                    if isinstance(test.output.stdout, dict):
                        test.output.stdout = ChannelData.from_dict(test.output.stdout)
                    if isinstance(test.output.stderr, dict):
                        test.output.stderr = ChannelData.from_dict(test.output.stderr)

    return plan


if __name__ == '__main__':
    with open('dsl/internal.json', 'r') as f:
        r = parse_test_plan(f)
