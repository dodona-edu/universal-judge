"""
Data format for the testplan.

This module is the authoritative source on the format and behaviour of the
testplan. Note that the implementation in the judge is kept simple by design;
unless noted, the judge will not provide default values for missing fields.
"""
from dataclasses import field
from enum import Enum
from typing import List, Optional, Union, Dict, Any, Literal

from pydantic.dataclasses import dataclass

from serialisation import Value


class TestPlanError(ValueError):
    """Error when the test plan is not valid."""
    pass


# Represents custom code. This is a mapping of the programming language to the actual code.
Code = Dict[str, str]


class FunctionType(str, Enum):
    TOP = "top"  # top level function
    STATIC = "static"  # static function
    INSTANCE = "instance"  # instance function
    MAIN = "main"  # Main function for running code


@dataclass
class FunctionCall:
    """Represents a function call"""
    type: FunctionType
    name: Optional[str]
    object: Optional[str]
    arguments: List[Value]


class Builtin(str, Enum):
    """List of built-in evaluators"""
    TEXT = "text"
    FILE = "file"
    VALUE = "value"


@dataclass
class BuiltinEvaluator:
    """
    Built-in evaluator in the judge. Some basic evaluators are available, as enumerated by
    :class:`Builtin`. These are useful for things like comparing text, files or values.

    This is the recommended and default evaluator, since it is a) the least amount of work and
    b) the most language independent.
    """
    # noinspection PyUnresolvedReferences
    type: Literal["builtin"]
    name: Builtin
    options: Dict[str, Any]  # Options for the evaluator


@dataclass
class CustomEvaluator:
    """
    Evaluate the responses with custom code. This is still a language-independent method; the
    evaluator is run as part of the judge and receives its values from that judge. This type is
    useful, for example, when doing exercises on sequence alignments.
    """
    # noinspection PyUnresolvedReferences
    type: Literal["custom"]
    language: str
    code: str


@dataclass
class SpecificEvaluator:
    """
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
    """
    # noinspection PyUnresolvedReferences
    type: Literal["specific"]
    evaluators: Code


class ChannelType(str, Enum):
    TEXT = "text"  # Literal values
    FILE = "file"  # Path to a file


@dataclass
class ChannelData:
    """Describes the data on channel (stdin or stdout)"""
    type: ChannelType
    data: str

    def get_data_as_string(self):
        if self.type == ChannelType.TEXT:
            return self.data
        elif self.type == ChannelType.FILE:
            try:
                with open(self.data, 'r') as f:
                    return f.read()
            except FileNotFoundError as e:
                raise TestPlanError(f"File not found: {e}")
        else:
            raise AssertionError(f"Unknown enum type {self.type}")


@dataclass
class TextOutputChannel(ChannelData):
    """Describes the output for textual channels."""
    evaluator: Union[BuiltinEvaluator, CustomEvaluator]


@dataclass
class FileOutputChannel:
    """Describes the output for files."""
    evaluator: Union[BuiltinEvaluator, CustomEvaluator]
    expected_path: str  # Path to the file to compare to.
    actual_path: str  # Path to the generated file (by the users code)


@dataclass
class ReturnOutputChannel:
    """Handles return values of function calls."""
    evaluator: Union[BuiltinEvaluator, CustomEvaluator, SpecificEvaluator]
    value: Optional[Value] = None


class NoneChannelState(str, Enum):
    NONE = "none"  # There is nothing on this channel, i.e. completely empty.


class IgnoredChannelState(str, Enum):
    """A file channel is ignored by default."""
    IGNORED = "ignored"


AnyChannelState = Union[NoneChannelState, IgnoredChannelState]


OutputChannel = Union[TextOutputChannel, FileOutputChannel, ReturnOutputChannel]


TextOutput = Union[TextOutputChannel, AnyChannelState]


@dataclass
class Testcase:
    """A test case is defined by an input and a set of tests."""
    description: Optional[str]  # Will be generated if None.
    essential: bool
    # Inputs
    function: FunctionCall
    stdin: Union[ChannelData, NoneChannelState]
    # Outputs
    stdout: TextOutput
    stderr: TextOutput
    file: Union[FileOutputChannel, IgnoredChannelState]
    result: Union[ReturnOutputChannel, IgnoredChannelState]

    def get_input(self):
        if self.stdin == NoneChannelState.NONE:
            return None
        else:
            return self.stdin.get_data_as_string()


@dataclass
class Context:
    """A context is what the name implies: executes things in the same context.
    As such, the context consists of three main things: the preparation, the
    execution and the post-processing.
    """
    execution: Testcase
    additional: List[Testcase]
    before: Code
    after: Code
    description: Optional[str] = None

    def all_testcases(self) -> List[Testcase]:
        return [self.execution] + self.additional


@dataclass
class Tab:
    """Represents a tab on Dodona."""
    name: str
    contexts: List[Context]


@dataclass
class Plan:
    """General test plan, which is used to run tests of some code."""
    tabs: List[Tab] = field(default_factory=list)


def parse_test_plan(json_string) -> Plan:
    """Parse a test plan into the structures."""
    plan = Plan.__pydantic_model__.parse_raw(json_string)
    return plan


if __name__ == '__main__':
    with open('../exercise/basic.json', 'r') as f:
        r = parse_test_plan(f.read())
        print(r)
