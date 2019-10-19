"""Testplan

This module is the authoritative source on the format and behaviour of the
testplan. Note that the implementation in the judge is kept simple by design;
unless noted, the judge will not provide default values for missing fields.
"""
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import List, Optional, Union
from dataclasses_json import dataclass_json


class TestPlanError(ValueError):
    """Error when the test plano is not valid."""
    pass


# Special class denoting the name of the object that must be run.
@dataclass_json
@dataclass
class Run:
    """Arguments necessary for loading the user's code into the context."""
    classname: Optional[str]


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
    data: Union[str, List[str]]
    type: Optional[DataType] = DataType.text


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
class Evaluators:
    stdout: Evaluator
    stderr: Evaluator
    file: Evaluator


@dataclass_json
@dataclass
class Test:
    """Describes a single test"""
    output: Output
    evaluators: Evaluators
    description: Optional[str]


@dataclass_json
@dataclass
class Testcase:
    """A test case is defined by an input and a set of tests."""
    input: Input
    tests: List[Test]
    description: Optional[str]  # Will be generated if None.


@dataclass_json
@dataclass
class Execution(Testcase):
    """Essentially the same as a normal testcase, but allows to specify if a
    main method should be executed or not for non-script languages (such as
    Java or C).
    """
    main: Run


@dataclass_json
@dataclass
class Context:
    """A context is what the name implies: executes things in the same context.
    As such, the context consists of three main things: the preparation, the
    execution and the post-processing.
    """
    execution: Execution
    postprocessing: List[Testcase]
    description: Optional[str] = None
    preparation: Optional[str] = None

    def all_testcases(self) -> List[Testcase]:
        return [self.execution] + self.postprocessing


@dataclass_json
@dataclass
class Tab:
    """Represents a tab on Dodona."""
    name: str
    contexts: List[Context]


@dataclass_json
@dataclass
class Plan:
    """General test plano, which is used to run tests of some code."""
    tabs: List[Tab] = field(default_factory=list)


def parse_test_plan(json_string) -> Plan:
    """Parse a test plano into the structures."""

    plan: Plan = Plan.from_json(json_string)
    plan.name = "Deprecated, will be removed."

    # Fix union types.
    for tab in plan.tabs:
        for context in tab.contexts:
            for testcase in context.all_testcases():
                if isinstance(testcase.input.stdin, dict):
                    testcase.input.stdin = ChannelData.from_dict(testcase.input.stdin)
                for test in testcase.tests:
                    if isinstance(test.output.stdout, dict):
                        test.output.stdout = ChannelData.from_dict(test.output.stdout)
                    if isinstance(test.output.stderr, dict):
                        test.output.stderr = ChannelData.from_dict(test.output.stderr)

    return plan


if __name__ == '__main__':
    with open('dsl/internal2.json', 'r') as f:
        r = parse_test_plan(f.read())
        print(r)


def _get_input(test: Testcase) -> List[str]:
    """Get the input for of a test"""
    if test.input.stdin == ChannelState.none.value:
        return []
    elif test.input.stdin.type == DataType.text:
        return test.input.stdin.data
    elif test.input.stdin.type == DataType.file:
        with open(test.input.stdin.data, "r") as file:
            return file.readlines()
    else:
        raise TestPlanError(f"Unknown input type in test plano: {test.input.stdin.type}")


def _get_stdout(test: Test) -> Optional[List[str]]:
    """Get the stdout value of a test"""
    if test.output.stdout == OutputChannelState.ignored.value:
        return None
    elif test.output.stdout == OutputChannelState.none.value:
        return []
    elif test.output.stdout.type == DataType.text:
        return test.output.stdout.data
    elif test.output.stdout.type == DataType.file:
        with open(test.output.stdout.data, "r") as file:
            return file.readlines()
    else:
        raise TestPlanError(f"Unknown output type in test plano: {test.output.stdout.type}")


def _get_stderr(test: Test) -> Optional[List[str]]:
    """Get the stderr value of a test"""
    if test.output.stderr == OutputChannelState.ignored:
        return None
    elif test.output.stderr == OutputChannelState.none.value:
        return []
    elif test.output.stderr.type == DataType.text:
        return test.output.stderr.data
    elif test.output.stderr.type == DataType.file:
        with open(test.output.stderr.data, "r") as file:
            return file.readlines()
    else:
        raise TestPlanError(f"Unknown stderr type in test plano: {test.output.stderr.type}")
