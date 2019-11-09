"""Testplan

This module is the authoritative source on the format and behaviour of the
testplan. Note that the implementation in the judge is kept simple by design;
unless noted, the judge will not provide default values for missing fields.
"""
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional, Union, Dict, Any

from dataclasses_json import config, DataClassJsonMixin


class TestPlanError(ValueError):
    """Error when the test plan is not valid."""
    pass


# Represents custom code. This is a mapping of the programming language to the actual code.
Code = Dict[str, str]


# region Functions
class ValueType(str, Enum):
    # Primitive types
    integer = "integer"  # Represents all integers
    rational = "rational"  # Represents all rational numbers
    text = "text"  # Represents textual data
    boolean = "boolean"
    # Others
    unknown = "unknown"  # We don't know the type of this value


@dataclass
class Value(DataClassJsonMixin):
    """Represents a value, which consists of the data and a type."""
    data: str
    type: ValueType


@dataclass
class FunctionArg(Value, DataClassJsonMixin):
    """Argument of a function"""
    name: Optional[str]  # Name of the argument, ignored if not supported by the language


class FunctionType(str, Enum):
    top = "top"  # top level function
    static = "static"  # static function
    instance = "instance"  # instance function
    main = "main"  # Main function for running code


@dataclass
class FunctionCall(DataClassJsonMixin):
    """Represents a function call"""
    type: FunctionType
    name: Optional[str]
    object: Optional[str]
    arguments: List[FunctionArg]
# endregion


# region Channels
class ChannelType(str, Enum):
    text = "text"  # Literal values
    file = "file"  # Path to a file


class ChannelState(str, Enum):
    none = "none"  # There is nothing on this channel, i.e. completely empty.


class OutputChannelState(str, Enum):
    none = "none"  # There is nothing on this channel, i.e. completely empty.
    ignored = "ignored"  # Ignore everything on this channel, i.e. doesn't matter what you use.


@dataclass
class ChannelData(DataClassJsonMixin):
    """Describes the data on channel (stdin or stdout)"""
    data: str
    type: ChannelType


@dataclass
class FileOutput(DataClassJsonMixin):
    """Describes output where the code should produce a file."""
    expected: str  # Path to the expected file
    actual: str  # Path to the generated file (by the users code)


@dataclass
class Input(DataClassJsonMixin):
    """Describes the input channels for a test"""
    stdin: Union[ChannelData, ChannelState]
    function: FunctionCall


@dataclass
class Output(DataClassJsonMixin):
    """Describes the output channels for a test"""
    stdout: Union[ChannelData, OutputChannelState]
    stderr: Union[ChannelData, OutputChannelState]
    file: Optional[FileOutput]
    result: Optional[Value]
# endregion


# region Evaluators
class EvaluatorType(str, Enum):
    """
    Evaluator types.

    Built-in
    --------
    Built-in evaluator in the judge. Some basic evaluators are available, as enumerated by
    :class:`Builtin`. These are useful for things like:

    - Comparing text
    - Comparing files
    - Comparing values (TODO)

    When using this type, you must supply a config of type :class:`BuiltinEvaluator`.

    Custom
    ------
    Evaluate the responses with custom code. This is still a language-independent method; the
    evaluator is run as part of the judge and receives its values from that judge. This type is
    useful, for example, when doing exercises on sequence alignments.

    When using this type, you must supply a config of type :class:`CustomEvaluator`.

    Specific
    --------
    Provide language-specific code that will be run in the same environment as the user's code.
    While this is very powerful and allows you to test language-specific constructs, there are a few
    caveats:

    1. The code is run alongside the user code. This means the user can potentially take control of
       the code.
    2. This will limit the number of language an exercise is available in, since you need to provide
       tests for all languages you want to support.
    3. It is a lot of work. You need to return the correct values, since the judge needs to
       understand what the result was.

    The code you must write should be a function that accepts the result of a user call.
    Note: this type of evaluator is only supported when using function calls. If you want to
    evaluate, say stdout, you should use the custom evaluator instead.

    When using this type, you must supply a config of type :class:`SpecificEvaluator`.
    """
    builtin = "builtin"
    custom = "custom"
    specific = "specific"


@dataclass
class BaseEvaluator(DataClassJsonMixin):
    """Base evaluator, not used directly."""
    type: EvaluatorType


class Builtin(str, Enum):
    """List of built-in evaluators"""
    text = "textEvaluator"
    file = "fileEvaluator"
    value = "valueEvaluator"


@dataclass
class BuiltinEvaluator(BaseEvaluator, DataClassJsonMixin):
    """Built-in evaluators"""
    name: Builtin
    options: Dict[str, Any]  # Options for the evaluator


@dataclass
class CustomEvaluator(BaseEvaluator, DataClassJsonMixin):
    """Evaluator using custom code."""
    language: str
    code: str


@dataclass
class SpecificEvaluator(BaseEvaluator, DataClassJsonMixin):
    """Evaluator using own evaluators."""
    evaluators: Code


def evaluator_parser(values: dict) -> Union[BuiltinEvaluator, CustomEvaluator]:
    base_type: BaseEvaluator = BaseEvaluator.from_dict(values)
    if base_type.type == EvaluatorType.builtin:
        return BuiltinEvaluator.from_dict(values)
    elif base_type.type == EvaluatorType.custom:
        return CustomEvaluator.from_dict(values)
    else:
        allowed = [x for x in EvaluatorType]
        raise TestPlanError(f"Unknown evaluator type {values}, should be one of {allowed}")


def specific_evaluator_parser(values: dict) -> Union[BuiltinEvaluator, CustomEvaluator, SpecificEvaluator]:
    base_type: BaseEvaluator = BaseEvaluator.from_dict(values)
    if base_type.type == EvaluatorType.specific:
        return SpecificEvaluator.from_dict(values)
    else:
        return evaluator_parser(values)


@dataclass
class Evaluators(DataClassJsonMixin):
    stdout: Union[BuiltinEvaluator, CustomEvaluator] = field(metadata=config(decoder=evaluator_parser))
    stderr: Union[BuiltinEvaluator, CustomEvaluator] = field(metadata=config(decoder=evaluator_parser))
    file: Union[BuiltinEvaluator, CustomEvaluator] = field(metadata=config(decoder=evaluator_parser))
    result: Union[BuiltinEvaluator, CustomEvaluator, SpecificEvaluator] = field(
        metadata=config(decoder=specific_evaluator_parser)
    )
# endregion


@dataclass
class Testcase(DataClassJsonMixin):
    """A test case is defined by an input and a set of tests."""
    input: Input
    output: Output
    evaluators: Evaluators
    description: Optional[str]  # Will be generated if None.


@dataclass
class Context(DataClassJsonMixin):
    """A context is what the name implies: executes things in the same context.
    As such, the context consists of three main things: the preparation, the
    execution and the post-processing.
    """
    execution: Testcase
    additional: List[Testcase]
    description: Optional[str] = None
    before: Optional[Code] = None
    after: Optional[Code] = None

    def all_testcases(self) -> List[Testcase]:
        return [self.execution] + self.additional


@dataclass
class Tab(DataClassJsonMixin):
    # noinspection PyUnresolvedReferences
    """Represents a tab on Dodona.

    :param name: OK
    """
    name: str
    contexts: List[Context]


@dataclass
class Plan(DataClassJsonMixin):
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
    import os
    cwd = os.getcwd()
    print(cwd)
    with open('../exercise/advanced.json', 'r') as f:
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
