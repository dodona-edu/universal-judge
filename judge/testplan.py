"""Testplan

This module is the authoritative source on the format and behaviour of the
testplan. Note that the implementation in the judge is kept simple by design;
unless noted, the judge will not provide default values for missing fields.
"""
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional, Union, Dict, Any, TypeVar, Type

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


class ChannelType(str, Enum):
    text = "text"  # Literal values
    file = "file"  # Path to a file


@dataclass
class ChannelData(DataClassJsonMixin):
    """Describes the data on channel (stdin or stdout)"""
    data: str
    type: ChannelType


class ChannelState(str, Enum):
    none = "none"  # There is nothing on this channel, i.e. completely empty.


class OutputChannelState(str, Enum):
    none = "none"  # There is nothing on this channel, i.e. completely empty.
    ignored = "ignored"  # Ignore everything on this channel, i.e. doesn't matter what you use.


class FileChannelState(str, Enum):
    ignored = "ignored"


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
    """Base evaluator, not used directly. Used to retrieve the type, and is then discarded."""
    type: EvaluatorType


class Builtin(str, Enum):
    """List of built-in evaluators"""
    text = "textEvaluator"
    file = "fileEvaluator"
    value = "valueEvaluator"


@dataclass
class BuiltinEvaluator(DataClassJsonMixin):
    """Built-in evaluators"""
    name: Builtin
    options: Dict[str, Any]  # Options for the evaluator


@dataclass
class CustomEvaluator(DataClassJsonMixin):
    """Evaluator using custom code."""
    language: str
    code: str


@dataclass
class SpecificEvaluator(DataClassJsonMixin):
    """Evaluator using own evaluators."""
    evaluators: Code


def _evaluator_parser(values: dict) -> Union[BuiltinEvaluator, CustomEvaluator]:
    base_type: BaseEvaluator = BaseEvaluator.from_dict(values)
    if base_type.type == EvaluatorType.builtin:
        return BuiltinEvaluator.from_dict(values)
    elif base_type.type == EvaluatorType.custom:
        return CustomEvaluator.from_dict(values)
    else:
        allowed = [x for x in EvaluatorType]
        raise TestPlanError(f"Unknown evaluator type {values}, should be one of {allowed}")


def _specific_evaluator_parser(values: dict) -> Union[BuiltinEvaluator, CustomEvaluator, SpecificEvaluator]:
    base_type: BaseEvaluator = BaseEvaluator.from_dict(values)
    if base_type.type == EvaluatorType.specific:
        return SpecificEvaluator.from_dict(values)
    else:
        return _evaluator_parser(values)


@dataclass
class OutputChannel(DataClassJsonMixin):
    evaluator: Any


@dataclass
class TextOutputChannel(ChannelData, OutputChannel, DataClassJsonMixin):
    """Describes the output for textual channels."""
    evaluator: Union[BuiltinEvaluator, CustomEvaluator] = field(metadata=config(decoder=_evaluator_parser))


@dataclass
class FileOutputChannel(OutputChannel, DataClassJsonMixin):
    """Describes the output for files."""
    evaluator: Union[BuiltinEvaluator, CustomEvaluator] = field(metadata=config(decoder=_evaluator_parser))
    expected_path: str  # Path to the file to compare to.
    actual_path: str  # Path to the generated file (by the users code)


@dataclass
class ReturnOutputChannel(Value, OutputChannel, DataClassJsonMixin):
    """Handles return values of function calls."""
    evaluator: Union[BuiltinEvaluator, CustomEvaluator, SpecificEvaluator] = field(
        metadata=config(decoder=_specific_evaluator_parser)
    )


_O = TypeVar('_O', bound=DataClassJsonMixin)


def _output_parser(value: Union[str, dict], r_type: Type[_O]) -> Union[_O, OutputChannelState]:
    if isinstance(value, str):
        return OutputChannelState[value]
    else:
        return r_type.from_dict(value)


def _file_output_parser(value: Union[str, dict]) -> Union[ReturnOutputChannel, FileChannelState]:
    if isinstance(value, str):
        return FileChannelState[value]
    else:
        return ReturnOutputChannel.from_dict(value)


def _input_parser(value: Union[str, dict]) -> Union[ChannelData, ChannelState]:
    if isinstance(value, str):
        return ChannelState[value]
    else:
        return ChannelData.from_dict(value)


# TODO: keep Input and Output classes or not?
@dataclass
class Input(DataClassJsonMixin):
    function: FunctionCall
    stdin: Union[ChannelData, ChannelState] = field(metadata=config(decoder=_input_parser))


@dataclass
class Output(DataClassJsonMixin):
    stdout: Union[TextOutputChannel, OutputChannelState] = field(
        metadata=config(decoder=lambda x: _output_parser(x, TextOutputChannel))
    )
    stderr: Union[TextOutputChannel, OutputChannelState] = field(
        metadata=config(decoder=lambda x: _output_parser(x, TextOutputChannel))
    )
    file: Union[FileOutputChannel, FileChannelState] = field(
        metadata=config(decoder=lambda x: _output_parser(x, FileOutputChannel))
    )
    result: Union[ReturnOutputChannel, OutputChannel] = field(
        metadata=config(decoder=_file_output_parser)
    )


@dataclass
class Testcase(DataClassJsonMixin):
    """A test case is defined by an input and a set of tests."""
    input: Input
    output: Output
    description: Optional[str]  # Will be generated if None.
    essential: bool


@dataclass
class Context(DataClassJsonMixin):
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

    return plan


if __name__ == '__main__':
    import os
    cwd = os.getcwd()
    print(cwd)
    with open('../exercise/basic.json', 'r') as f:
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
