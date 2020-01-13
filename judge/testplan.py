"""
Data format for the testplan.

This module is the authoritative source on the format and behaviour of the
testplan. Note that the implementation in the judge is kept simple by design;
unless noted, the judge will not provide default values for missing fields.
"""
from dataclasses import field
from enum import Enum
from os import path

from humps import is_camelcase
from pydantic import validator, root_validator
from pydantic.dataclasses import dataclass
from typing import List, Optional, Union, Dict, Any, Literal

from serialisation import Value, ExceptionValue, NumericTypes, StringTypes, BooleanTypes, ObjectTypes, SequenceTypes, \
    NothingTypes, InstanceTypes


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
    FUNCTION = "function"  # Normal function call.
    CONSTRUCTOR = "constructor"  # Will be a constructor; the "object" is ignored.
    IDENTITY = "identity"  # Must have one argument, cannot have object


@dataclass
class FunctionCall:
    """Represents a function call"""
    type: FunctionType
    name: Optional[str] = None
    object: Optional[str] = None
    arguments: List[Value] = field(default_factory=list)

    @staticmethod
    @validator('name')
    def is_camelcase(cls, value):
        if not is_camelcase(value):
            return ValueError(f"Function {value} should be in camelCase.")
        return value

    @root_validator
    def identity_cannot_have_object(cls, values):
        type_ = values.get("type")
        object_ = values.get("object")
        if type_ == FunctionType.IDENTITY and object_ is not None:
            raise ValueError(f"An identity call cannot have an object, but got {object_}")
        return values

    @root_validator
    def identity_needs_one_argument(cls, values):
        arguments = values.get("arguments")
        type_ = values.get("type")
        if type_ == FunctionType.IDENTITY and len(arguments) != 1:
            raise ValueError(f"Identity call requires exactly one argument, but got {arguments}")
        return values

    @root_validator
    def non_identity_needs_name(cls, values):
        type_ = values.get("type")
        name_ = values.get("name")
        if type_ != FunctionType.IDENTITY and not name_:
            raise ValueError("Non-identity functions must have a name.")
        return values


VariableTypes = Union[
    # Types from the serialization format
    NumericTypes,
    StringTypes,
    BooleanTypes,
    SequenceTypes,
    ObjectTypes,
    NothingTypes,
    # Allow instance types
    InstanceTypes
]


@dataclass
class VariableType:
    type: VariableTypes
    data: Optional[str] = None


@dataclass
class Assignment:
    """
    Assigns the return value of a function to a variable. Because the expression part is pretty
    simple, the type of the value is determined by looking at the expression. It is also possible
    to define the type. If the type cannot be determined and it is not specified, this is an error.
    """
    name: str
    expression: FunctionCall
    type: Optional[VariableType] = None

    def get_type(self) -> VariableType:
        """
        Get the type of the variable. If a type is specified, it will be returned. Otherwise, this
        functions tries to determine type in a best-efforts manner. Currently, expressions with a
        function of type "identity" or "constructor" can be determined. This function does not check
        the given type against a determined type; if the given type is incompatible, this will lead
        to a crash during the execution.

        :return: The type, and optionally some data about the type. The data is currently only used
        when the type is an instance, which will then contain the name of the instance.
        """
        if self.type:
            return self.type
        if self.expression.type == FunctionType.IDENTITY:
            arg = self.expression.arguments[0]
            return VariableType(arg.type)
        if self.expression.type == FunctionType.CONSTRUCTOR:
            class_name = self.expression.arguments[0].data
            return VariableType(InstanceTypes.INSTANCE, class_name)

        raise TestPlanError(f"Could not determine type of variable {self.name}")

    def replace_function(self, function: FunctionCall) -> 'Assignment':
        return Assignment(name=self.name, expression=function, type=self.type)


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
    A built-in evaluator in the judge. Some basic evaluators are available, as enumerated by
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
class Input:
    """The input channels for a testcase."""
    stdin: Union[TextData, NoneChannelState] = NoneChannelState.NONE

    def get_as_string(self, working_directory):
        if self.stdin == NoneChannelState.NONE:
            return None
        else:
            return self.stdin.get_data_as_string(working_directory)


@dataclass
class Output:
    """The output channels for a testcase."""
    stdout: TextOutput = IgnoredChannelState.IGNORED
    stderr: TextOutput = NoneChannelState.NONE
    file: Union[FileOutputChannel, IgnoredChannelState] = IgnoredChannelState.IGNORED
    exception: Union[ExceptionOutputChannel, AnyChannelState] = NoneChannelState.NONE
    result: Union[ValueOutputChannel, IgnoredChannelState, NoneChannelState] = IgnoredChannelState.IGNORED


@dataclass
class MainInput(Input):
    """Input for the main testcase."""
    arguments: List[Value] = field(default_factory=list)  # Main args of the program.


@dataclass
class _RequiredFunctionInput:
    function: FunctionCall


@dataclass
class FunctionInput(Input, _RequiredFunctionInput):
    """Input channel where there is a function call as input."""
    pass


@dataclass
class _RequiredAssignmentInput:
    assignment: Assignment


@dataclass
class AssignmentInput(Input, _RequiredAssignmentInput):
    """Input channel where there is an assignment as input."""
    pass


NormalInput = Union[FunctionInput, AssignmentInput]


@dataclass
class Testcase:
    """A testcase is defined by an input channel and an output channel"""
    description: Optional[str] = None  # Will be generated if None.
    essential: bool = True
    input: Input = Input()  # This value is never used, it is useful for MRO issues only.
    output: Output = Output()


@dataclass
class MainTestcase(Testcase):
    """
    The main testcase for a context. Responsible for calling a main function if necessary, but also
    for providing the main arguments, and the stdin for scripts.
    """
    input: MainInput = MainInput()


@dataclass
class NormalTestcase(Testcase):
    """
    A normal testcase for a context. This testcase is responsible for calling functions and running
    code to test.
    """
    input: NormalInput

    @root_validator
    def no_return_with_assignment(cls, values):
        input_ = values.get("input")
        output_ = values.get("output")
        if isinstance(input_, AssignmentInput) \
                and not (output_.result == IgnoredChannelState.IGNORED or output_.result == NoneChannelState.NONE):
            raise ValueError(f"An assignment cannot have a return value.")
        return values


class NoMainTestcase(str, Enum):
    NONE = "none"


@dataclass
class Context:
    """
    A context corresponds to a context as defined by the Dodona test format.
    It is a collection of testcases that are run together, without isolation.

    A context should consist of at least one testcase: either the main testcase, or if needed,
    normal testcases.

    Note that in many cases, there might be just one testcase. For example, if the context is used
    to test one function, the context will contain one function testcase, and nothing more.
    """
    main: Union[MainTestcase, NoMainTestcase] = NoMainTestcase.NONE
    normal: List[NormalTestcase] = field(default_factory=list)
    before: Code = field(default_factory=dict)
    after: Code = field(default_factory=dict)
    description: Optional[str] = None

    def all_testcases(self) -> List[Testcase]:
        if self.main == NoMainTestcase.NONE:
            return self.normal
        else:
            # noinspection PyTypeChecker
            return [self.main] + self.normal

    @root_validator
    def check_testcases_exist(cls, values):
        execution = values.get('main')
        additional = values.get('normal')
        if execution == NoMainTestcase.NONE and not additional:
            raise ValueError("Either main or normal testcases must be defined.")
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
    object: str = "Main"


def parse_test_plan(json_string) -> Plan:
    """Parse a test plan into the structures."""
    plan = Plan.__pydantic_model__.parse_raw(json_string)
    return plan


if __name__ == '__main__':
    with open('../lotto/plan.json', 'r') as f:
        r = parse_test_plan(f.read())
        print(r)
