"""
Data format for the testplan.

This module is the authoritative source on the format and behaviour of the
testplan. Note that the implementation in the judge is kept simple by design;
unless noted, the judge will not provide default values for missing fields.
"""
from dataclasses import field
from enum import Enum
from os import path

from typing import List, Optional, Union, Dict, Any, Literal

from humps import is_camelcase
from pydantic import validator, root_validator
from pydantic.dataclasses import dataclass

from serialisation import Value, ExceptionValue


class TestPlanError(ValueError):
    """Error when the test plan is not valid."""
    pass


class ChannelType(str, Enum):
    TEXT = "text"  # Literal values
    FILE = "file"  # Path to a file


@dataclass
class TextData:
    """Describes textual data: either directly or in a file."""
    data: str
    type: ChannelType = ChannelType.TEXT

    @staticmethod
    def __resolve_path(working_directory, file_path):
        """
        Resolve a path to an absolute path. Relative paths will be resolved against the given
        ``working_directory``, not the actual working directory.
        """
        if path.isabs(file_path):
            return path.abspath(file_path)
        else:
            return path.abspath(path.join(working_directory, file_path))

    def get_data_as_string(self, working_directory):
        """Get the data as a string, reading the file if necessary."""
        if self.type == ChannelType.TEXT:
            return self.data
        elif self.type == ChannelType.FILE:
            try:
                file_path = self.__resolve_path(working_directory, self.data)
                with open(file_path, 'r') as file:
                    return file.read()
            except FileNotFoundError as e:
                raise TestPlanError(f"File not found: {e}")
        else:
            raise AssertionError(f"Unknown enum type {self.type}")


class FunctionType(str, Enum):
    TOP = "top"  # top level function
    OBJECT = "static"  # function on an object or class


@dataclass
class FunctionCall:
    """Represents a function call"""
    type: FunctionType
    name: str
    object: Optional[str] = None
    arguments: List[Value] = field(default_factory=list)

    @staticmethod
    @validator('name')
    def is_camelcase(cls, value):
        if not is_camelcase(value):
            return ValueError(f"Function {value} should be in camelCase.")
        return value


class TextBuiltin(str, Enum):
    """List of built-in evaluators"""
    TEXT = "text"
    FILE = "file"


class ValueBuiltin(str, Enum):
    VALUE = "value"


class ExceptionBuiltin(str, Enum):
    EXCEPTION = "exception"


@dataclass
class BaseBuiltinEvaluator:
    """
    Built-in evaluator in the judge. Some basic evaluators are available, as enumerated by
    :class:`Builtin`. These are useful for things like comparing text, files or values.

    This is the recommended and default evaluator, since it is a) the least amount of work and
    b) the most language independent.
    """
    # noinspection PyUnresolvedReferences
    type: Literal["builtin"] = "builtin"
    options: Dict[str, Any] = field(default_factory=dict)  # Options for the evaluator


@dataclass
class TextBuiltinEvaluator(BaseBuiltinEvaluator):
    name: TextBuiltin = TextBuiltin.TEXT


@dataclass
class ValueBuiltinEvaluator(BaseBuiltinEvaluator):
    name: ValueBuiltin = ValueBuiltin.VALUE


@dataclass
class ExceptionBuiltinEvaluator(BaseBuiltinEvaluator):
    name: ExceptionBuiltin = ExceptionBuiltin.EXCEPTION


@dataclass
class CustomEvaluator:
    """
    Evaluate the responses with custom code. This is still a language-independent method; the
    evaluator is run as part of the judge and receives its values from that judge. This type is
    useful, for example, when doing exercises on sequence alignments.
    """
    language: str
    code: TextData
    # noinspection PyUnresolvedReferences
    type: Literal["custom"] = "custom"


# Represents custom code. This is a mapping of the programming language to the actual code.
Code = Dict[str, TextData]


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
    evaluators: Code
    # noinspection PyUnresolvedReferences
    type: Literal["specific"] = "specific"


@dataclass
class TextOutputChannel(TextData):
    """Describes the output for textual channels."""
    evaluator: Union[TextBuiltinEvaluator, CustomEvaluator] = TextBuiltinEvaluator()


@dataclass
class FileOutputChannel:
    """Describes the output for files."""
    expected_path: str  # Path to the file to compare to.
    actual_path: str  # Path to the generated file (by the users code)
    evaluator: Union[TextBuiltinEvaluator, CustomEvaluator] = TextBuiltinEvaluator(name=TextBuiltin.FILE)


@dataclass
class ValueOutputChannel:
    """Handles return values of function calls."""
    value: Optional[Value] = None
    evaluator: Union[ValueBuiltinEvaluator, CustomEvaluator, SpecificEvaluator] = ValueBuiltinEvaluator()


@dataclass
class ExceptionOutputChannel:
    """Handles exceptions of user code if needed."""
    exception: ExceptionValue
    evaluator: Union[ExceptionBuiltinEvaluator, CustomEvaluator, SpecificEvaluator] = ExceptionBuiltinEvaluator()


class NoneChannelState(str, Enum):
    NONE = "none"  # There is nothing on this channel, i.e. completely empty.


class IgnoredChannelState(str, Enum):
    """A file channel is ignored by default."""
    IGNORED = "ignored"


AnyChannelState = Union[NoneChannelState, IgnoredChannelState]


OutputChannel = Union[TextOutputChannel, FileOutputChannel, ValueOutputChannel]


TextOutput = Union[TextOutputChannel, AnyChannelState]


@dataclass
class Testcase:
    """A test case is defined by an input and a set of tests."""
    # General stuff
    description: Optional[str] = None  # Will be generated if None.
    essential: bool = True
    # Inputs
    stdin: Union[TextData, NoneChannelState] = NoneChannelState.NONE
    # Outputs
    stdout: TextOutput = IgnoredChannelState.IGNORED
    stderr: TextOutput = NoneChannelState.NONE
    file: Union[FileOutputChannel, IgnoredChannelState] = IgnoredChannelState.IGNORED
    exception: Union[ExceptionOutputChannel, AnyChannelState] = NoneChannelState.NONE

    def get_input(self, working_directory):
        if self.stdin == NoneChannelState.NONE:
            return None
        else:
            return self.stdin.get_data_as_string(working_directory)


@dataclass
class _RequiredAdditional:
    """Internal helper, needed to work around MRO issues."""
    function: FunctionCall  # Function call for the testcase.
    result: Union[ValueOutputChannel, IgnoredChannelState, NoneChannelState]  # Result of the testcase.


@dataclass
class ExecutionTestcase(Testcase):
    """Main testcase for a context."""
    arguments: List[Value] = field(default_factory=list)  # Main args of the program.
    result: IgnoredChannelState = IgnoredChannelState.IGNORED  # Needed internally.


@dataclass
class AdditionalTestcase(Testcase, _RequiredAdditional):
    """Additional test case."""
    pass


class NoExecutionTestcase(str, Enum):
    NONE = "none"


@dataclass
class Context:
    """
    A context corresponds to a context as defined by the Dodona test format.
    It is a collection of testcases that are run together, without isolation.
    """
    execution: Union[ExecutionTestcase, NoExecutionTestcase] = NoExecutionTestcase.NONE
    additional: List[AdditionalTestcase] = field(default_factory=list)
    before: Code = field(default_factory=dict)
    after: Code = field(default_factory=dict)
    object: str = "Main"
    description: Optional[str] = None

    def all_testcases(self) -> List[Testcase]:
        if self.execution == NoExecutionTestcase.NONE:
            return self.additional
        else:
            # noinspection PyTypeChecker
            return [self.execution] + self.additional

    @root_validator
    def check_testcases_exist(cls, values):
        execution = values.get('execution')
        additional = values.get('additional')
        if execution == NoExecutionTestcase.NONE and not additional:
            raise ValueError("Either execution or additional test cases must be defined.")
        return values


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
    with open('../exercise/full.json', 'r') as f:
        original = parse_test_plan(f.read())

    with open('../exercise/short.json', 'r') as f:
        r = parse_test_plan(f.read())
        print(r)
        print(f"IS CORRECT? {original == r}")
