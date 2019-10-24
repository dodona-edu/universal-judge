"""Testplan

This module is the authoritative source on the format and behaviour of the
testplan. Note that the implementation in the judge is kept simple by design;
unless noted, the judge will not provide default values for missing fields.
"""
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional, Union
from dataclasses_json import dataclass_json


class TestPlanError(ValueError):
    """Error when the test plan is not valid."""
    pass


# region Functions
class ValueType(Enum):
    # Primitive types
    integer = "integer"  # Represents all integers
    rational = "rational"  # Represents all rational numbers
    text = "text"  # Represents textual data
    boolean = "boolean"
    # Others
    unknown = "unknown"  # We don't know the type of this value


@dataclass_json
@dataclass
class Value:
    """Represents a value, which consists of the data and a type."""
    data: str
    type: ValueType


@dataclass_json
@dataclass
class FunctionArg(Value):
    """Argument of a function"""
    name: Optional[str]  # Name of the argument, ignored if not supported by the language


class FunctionType(Enum):
    top = "top"  # top level function
    static = "static"  # static function
    instance = "instance"  # instance function
    main = "main"  # Main function for running code


@dataclass_json
@dataclass
class FunctionCall:
    """Represents a function call"""
    type: FunctionType
    name: Optional[str]
    object: str
    arguments: List[FunctionArg]
# endregion


# region Channels
class ChannelType(Enum):
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
    data: str
    type: ChannelType


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
    stdin: Union[ChannelData, ChannelState]
    function: FunctionCall


@dataclass_json
@dataclass
class Output:
    """Describes the output channels for a test"""
    stdout: Union[ChannelData, OutputChannelState]
    stderr: Union[ChannelData, OutputChannelState]
    file: Optional[FileOutput]
    result: Optional[Value]
# endregion


# region Evaluators
class EvaluatorType(Enum):
    builtin = "builtin"
    external = "external"


TEXT_COMPARATOR = "textComparator"
TEXT_ARGUMENTS = ["ignoreWhitespace"]
FILE_COMPARATOR = "fileComparator"
VALUE_COMPARATOR = "valueComparator"


@dataclass_json
@dataclass
class Evaluator:
    """Will evaluate the test."""
    name: str  # Name of the comparator or path to the evaluator.
    type: EvaluatorType  # Type of the evaluator.
    language: Optional[str]  # Ignored when using built-in evaluators
    options: List[str]  # Pass options to the evaluator.


@dataclass_json
@dataclass
class Evaluators:
    stdout: Evaluator
    stderr: Evaluator
    file: Evaluator
    result: Evaluator
# endregion


@dataclass_json
@dataclass
class Testcase:
    """A test case is defined by an input and a set of tests."""
    input: Input
    output: Output
    evaluators: Evaluators
    description: Optional[str]  # Will be generated if None.


@dataclass_json
@dataclass
class Context:
    """A context is what the name implies: executes things in the same context.
    As such, the context consists of three main things: the preparation, the
    execution and the post-processing.
    """
    execution: Testcase
    additional: List[Testcase]
    description: Optional[str] = None

    def all_testcases(self) -> List[Testcase]:
        return [self.execution] + self.additional


@dataclass_json
@dataclass
class Tab:
    # noinspection PyUnresolvedReferences
    """Represents a tab on Dodona.

    :param name: OK
    """
    name: str
    contexts: List[Context]


@dataclass_json
@dataclass
class Plan:
    """General test plan, which is used to run tests of some code."""
    tabs: List[Tab] = field(default_factory=list)


def parse_test_plan(json_string) -> Plan:
    """Parse a test plan into the structures."""

    plan: Plan = Plan.from_json(json_string)
    plan.name = "Deprecated, will be removed."

    # Fix union types.
    for tab in plan.tabs:
        for context in tab.contexts:
            for testcase in context.all_testcases():
                if isinstance(testcase.input.stdin, dict):
                    testcase.input.stdin = ChannelData.from_dict(testcase.input.stdin)
                elif isinstance(testcase.input.stdin, str):
                    testcase.input.stdin = ChannelState[testcase.input.stdin]
                if isinstance(testcase.output.stdout, dict):
                    testcase.output.stdout = ChannelData.from_dict(testcase.output.stdout)
                elif isinstance(testcase.output.stdout, str):
                    testcase.output.stdout = OutputChannelState[testcase.output.stdout]
                if isinstance(testcase.output.stderr, dict):
                    testcase.output.stderr = ChannelData.from_dict(testcase.output.stderr)
                elif isinstance(testcase.output.stderr, str):
                    testcase.output.stderr = OutputChannelState[testcase.output.stderr]

    return plan


if __name__ == '__main__':
    with open('dsl/internal2.json', 'r') as f:
        r = parse_test_plan(f.read())
        print(r)
        print(r.to_json())


def _get_stdin(test: Testcase) -> str:
    """Get the input for of a test as text."""
    if test.input.stdin == ChannelState.none:
        return ""
    elif test.input.stdin.type == ChannelType.text:
        return test.input.stdin.data
    elif test.input.stdin.type == ChannelType.file:
        with open(test.input.stdin.data[0], "r") as file:
            return file.read()
    else:
        raise TestPlanError(f"Unknown input type in test plano: {test.input.stdin.type}")


def _get_stdout(test: Testcase) -> Optional[str]:
    """Get the stdout value of a test as text."""
    if test.output.stdout == OutputChannelState.ignored.value:
        return None
    elif test.output.stdout == OutputChannelState.none.value:
        return ""
    elif test.output.stdout.type == ChannelType.text:
        return test.output.stdout.data
    elif test.output.stdout.type == ChannelType.file:
        with open(test.output.stdout.data[0], "r") as file:
            return file.read()
    else:
        raise TestPlanError(f"Unknown output type in test plano: {test.output.stdout.type}")


def _get_stderr(test: Testcase) -> Optional[str]:
    """Get the stderr value of a test as text."""
    if test.output.stderr == OutputChannelState.ignored:
        return None
    elif test.output.stderr == OutputChannelState.none:
        return ""
    elif test.output.stderr.type == ChannelType.text:
        return test.output.stderr.data
    elif test.output.stderr.type == ChannelType.file:
        with open(test.output.stderr.data[0], "r") as file:
            return file.read()
    else:
        raise TestPlanError(f"Unknown stderr type in test plano: {test.output.stderr.type}")
