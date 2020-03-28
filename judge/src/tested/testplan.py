"""
Structures that model the testplan in code.

This module is the authoritative source on the format and behaviour of the testplan.
When executing this module, a json-schema is generated for the format, which can be
of assistance when checking existing testplans.
"""
from collections import defaultdict
from dataclasses import field
from enum import Enum
from os import path
from pathlib import Path
from typing import List, Optional, Dict, Any, Literal, Union, NamedTuple, Iterable

from pydantic import BaseModel, root_validator, validator
from pydantic.dataclasses import dataclass

from .datatypes import StringTypes
from .features import (Constructs, FeatureSet, combine_features, WithFeatures,
                       NOTHING)
from .serialisation import (ExceptionValue, Value, Expression, Statement,
                            Identifier, FunctionCall, SequenceType, ObjectType,
                            FunctionType, WithFunctions)
from .utils import get_args, flatten


class TextBuiltin(str, Enum):
    """Textual built in evaluators."""
    TEXT = "text"
    FILE = "file"


class ValueBuiltin(str, Enum):
    """Built in evaluators for values."""
    VALUE = "value"


class ExceptionBuiltin(str, Enum):
    """Built in evaluators for exceptions."""
    EXCEPTION = "exception"


@dataclass
class BaseBuiltinEvaluator:
    """
    A built-in evaluator in TESTed. Some basic evaluators are available, as
    enumerated by :class:`Builtin`. These are useful for things like comparing text,
    files or values.

    This is the recommended and default evaluator, since it is a) the least amount
    of work and b) the most language independent.
    """
    type: Literal["builtin"] = "builtin"
    options: Dict[str, Any] = field(default_factory=dict)


@dataclass
class GenericTextEvaluator(BaseBuiltinEvaluator):
    name: TextBuiltin = TextBuiltin.TEXT


@dataclass
class GenericValueEvaluator(BaseBuiltinEvaluator):
    name: ValueBuiltin = ValueBuiltin.VALUE


@dataclass
class GenericExceptionEvaluator(BaseBuiltinEvaluator):
    name: ExceptionBuiltin = ExceptionBuiltin.EXCEPTION


@dataclass
class ProgrammedEvaluator:
    """
    Evaluate the responses with custom code. This is still a language-independent
    method; the evaluator is run as part of the judge and receives its values from
    that judge. This type is useful, for example, when doing exercises on sequence
    alignments.

    TODO: the custom evaluator should be able to access the input of the testcase.
      How should we handle functions? Stdin? Stdout?
    """
    language: str
    path: Path
    arguments: List[Value] = field(default_factory=list)
    type: Literal["custom"] = "custom"


@dataclass
class SpecificEvaluator:
    """
    Provide language-specific code that will be run in the same environment as the
    user's code. While this is very powerful and allows you to test language-
    specific constructs, there are a few caveats:

    1. The code is run alongside the user code. This means the user can potentially
       take control of the code.
    2. This will limit the context_number of language an exercise is available in,
       since you need to provide tests for all configs you want to support.
    3. It is a lot of work. You need to return the correct values, since the judge
       needs to understand what the result was.

    The code you must write should be a function that accepts the result of a user
    expression. Note: this type of evaluator is only supported when using function
    calls. If you want to evaluate_text stdout you should use the custom evaluator
    instead.
    """
    evaluators: Dict[str, Path]
    type: Literal["specific"] = "specific"

    def for_language(self, language: str) -> Path:
        return self.evaluators[language]


class TextChannelType(str, Enum):
    TEXT = "text"  # Literal values
    FILE = "file"  # Path to a file


@dataclass
class TextData(WithFeatures):
    """Describes textual data: either directly or in a file."""

    data: str
    type: TextChannelType = TextChannelType.TEXT

    @staticmethod
    def __resolve_path(working_directory, file_path):
        """
        Resolve a path to an absolute path. Relative paths will be resolved against
        the given ``directory``, not the actual working directory.
        """
        if path.isabs(file_path):
            return path.abspath(file_path)
        else:
            return path.abspath(path.join(working_directory, file_path))

    def get_data_as_string(self, working_directory):
        """Get the data as a string, reading the file if necessary."""
        if self.type == TextChannelType.TEXT:
            return self.data
        elif self.type == TextChannelType.FILE:
            file_path = self.__resolve_path(working_directory, self.data)
            with open(file_path, 'r') as file:
                return file.read()
        else:
            raise AssertionError(f"Unknown enum type {self.type}")

    def get_used_features(self) -> FeatureSet:
        return NOTHING


@dataclass
class TextOutputChannel(TextData):
    """Describes the output for textual channels."""
    evaluator: Union[
        GenericTextEvaluator, ProgrammedEvaluator
    ] = GenericTextEvaluator()


@dataclass
class FileOutputChannel(WithFeatures):
    """Describes the output for files."""
    expected_path: str  # Path to the file to compare to.
    actual_path: str  # Path to the generated file (by the users code)
    evaluator: Union[
        GenericTextEvaluator, ProgrammedEvaluator
    ] = GenericTextEvaluator(name=TextBuiltin.FILE)

    def get_used_features(self) -> FeatureSet:
        return NOTHING


@dataclass
class ValueOutputChannel(WithFeatures):
    """Handles return values of function calls."""
    value: Optional[Value] = None
    evaluator: Union[
        GenericValueEvaluator, ProgrammedEvaluator, SpecificEvaluator
    ] = GenericValueEvaluator()

    def get_used_features(self) -> FeatureSet:
        if self.value:
            return self.value.get_used_features()
        return NOTHING

    @root_validator
    def value_requirements(cls, values):
        value = values.get("value")
        evaluator = values.get("evaluator")
        if isinstance(evaluator, GenericValueEvaluator) and not value:
            raise ValueError("The generic evaluator needs an channel value.")
        return values


@dataclass
class ExceptionOutputChannel(WithFeatures):
    """Handles exceptions caused by the submission."""
    exception: Optional[ExceptionValue] = None
    evaluator: Union[
        GenericExceptionEvaluator, SpecificEvaluator
    ] = GenericExceptionEvaluator()

    def get_used_features(self) -> FeatureSet:
        if self.exception:
            return self.exception.get_used_features()
        return NOTHING

    @root_validator
    def value_requirements(cls, values):
        exception = values.get("exception")
        evaluator = values.get("evaluator")
        if isinstance(evaluator, GenericExceptionEvaluator) and not exception:
            raise ValueError("The generic evaluator needs an channel exception.")
        return values


@dataclass
class ExitCodeOutputChannel(WithFeatures):
    """Handles exit codes."""
    value: int = 0

    def get_used_features(self) -> FeatureSet:
        return NOTHING


class EmptyChannel(WithFeatures, str, Enum):
    """There is nothing on this output channel."""
    NONE = "none"

    def get_used_features(self) -> FeatureSet:
        return NOTHING


class IgnoredChannel(WithFeatures, str, Enum):
    """A file channel is ignored by default."""
    IGNORED = "ignored"

    def get_used_features(self) -> FeatureSet:
        return NOTHING


SpecialOutputChannel = Union[EmptyChannel, IgnoredChannel]

NormalOutputChannel = Union[
    TextOutputChannel,
    FileOutputChannel,
    ValueOutputChannel,
    ExitCodeOutputChannel
]

OutputChannel = Union[NormalOutputChannel, SpecialOutputChannel]


@dataclass
class ExpressionInput(WithFeatures, WithFunctions):
    """Input for an expression."""
    expression: Expression

    def get_used_features(self) -> FeatureSet:
        return self.expression.get_used_features()

    def get_functions(self) -> Iterable[FunctionCall]:
        return self.expression.get_functions()


@dataclass
class StatementInput(WithFeatures, WithFunctions):
    """Input for a command."""
    statement: Statement

    def get_used_features(self) -> FeatureSet:
        return self.statement.get_used_features()

    def get_functions(self) -> Iterable[FunctionCall]:
        return self.statement.get_functions()


_TextOutput = Union[TextOutputChannel, SpecialOutputChannel]
_FileOutput = Union[FileOutputChannel, IgnoredChannel]
_ExceptionOutput = Union[ExceptionOutputChannel, SpecialOutputChannel]
_ValueOutput = Union[ValueOutputChannel, IgnoredChannel]


@dataclass
class BaseOutput(WithFeatures):
    """The output channels for a testcase."""

    stdout: _TextOutput = EmptyChannel.NONE
    stderr: _TextOutput = EmptyChannel.NONE
    file: _FileOutput = IgnoredChannel.IGNORED
    exception: _ExceptionOutput = EmptyChannel.NONE

    def get_used_features(self) -> FeatureSet:
        return combine_features([
            self.stdout.get_used_features(),
            self.stderr.get_used_features(),
            self.file.get_used_features(),
            self.exception.get_used_features(),
        ])


@dataclass
class Output(BaseOutput):
    """The output channels for a testcase."""
    result: _ValueOutput = IgnoredChannel.IGNORED

    def get_used_features(self) -> FeatureSet:
        return combine_features([
            super().get_used_features(),
            self.result.get_used_features()
        ])

    @validator("result")
    def validate_no_expression(cls, v):
        """
        The values in the "result" output channel can never contain an expression,
        only a value. Since the serialisation format has become too permissive, we
        restrict it using a validator.
        """
        def _only_values(value: Expression) -> bool:
            if isinstance(value, Identifier):
                return False
            if isinstance(value, FunctionCall):
                return False
            if isinstance(value, SequenceType):
                return all(_only_values(va) for va in value.data)
            if isinstance(value, ObjectType):
                return all(_only_values(va) for k, va in value.data.items())

            return True

        if v == IgnoredChannel.IGNORED:
            return v
        assert isinstance(v, ValueOutputChannel)
        if not _only_values(v.value):
            raise ValueError("Only values are allowed in descriptive mode.")
        return v


@dataclass
class Testcase(WithFeatures, WithFunctions):
    """A testcase is defined by an input channel and an output channel"""
    input: Union[ExpressionInput, StatementInput]
    description: Optional[str] = None  # Will be generated if None.
    essential: bool = True
    output: Output = Output()

    def get_used_features(self) -> FeatureSet:
        return combine_features(
            [self.input.get_used_features(), self.output.get_used_features()]
        )

    @root_validator
    def no_return_with_assignment(cls, values):
        input_ = values.get("input")
        output_ = values.get("output")
        if isinstance(input_, StatementInput) and not (
                output_.result == IgnoredChannel.IGNORED
                or output_.result == EmptyChannel.NONE
        ):
            raise ValueError(f"An convert_statement cannot have a return value.")
        return values

    def get_functions(self) -> Iterable[FunctionCall]:
        return self.input.get_functions()


@dataclass
class ContextInput(WithFeatures):
    """Input at a context-level."""

    stdin: Union[TextData, EmptyChannel] = EmptyChannel.NONE
    arguments: List[str] = field(default_factory=list)
    main_call: bool = False

    def get_as_string(self, working_directory):
        if self.stdin == EmptyChannel.NONE:
            return None
        else:
            return self.stdin.get_data_as_string(working_directory)

    def get_used_features(self) -> FeatureSet:
        if self.arguments:
            return FeatureSet(Constructs.NOTHING, {StringTypes.TEXT})
        else:
            return NOTHING


@dataclass
class ContextOutput(BaseOutput):
    """Output on a context level."""
    exit_code: ExitCodeOutputChannel = ExitCodeOutputChannel()


@dataclass
class ContextTestcase(WithFeatures):
    """
    The context test case. This test case contains various things related to the
    context itself, such as:
    - Execution of the context_testcase function.
    - Exit codes.
    - Standard input.
    """
    input: ContextInput = ContextInput()
    output: ContextOutput = ContextOutput()
    description: Optional[str] = None

    def get_used_features(self) -> FeatureSet:
        return combine_features([
            self.input.get_used_features(),
            self.output.get_used_features()
        ])


Code = Dict[str, TextData]


@dataclass
class Context(WithFeatures, WithFunctions):
    """
    A test case is an independent run of the solution.
    """
    context_testcase: ContextTestcase = ContextTestcase()
    testcases: List[Testcase] = field(default_factory=list)
    before: Code = field(default_factory=dict)
    after: Code = field(default_factory=dict)
    description: Optional[str] = None

    @root_validator
    def check_testcases_exist(cls, values):
        context = values.get('context_testcase')
        additional = values.get('testcases')
        if not context.input.main_call and not additional:
            raise ValueError("A context needs a context testcase or at least one "
                             "normal testcase.")
        return values

    def get_used_features(self) -> FeatureSet:
        return combine_features([
            self.context_testcase.get_used_features(),
            combine_features([x.get_used_features() for x in self.testcases])
        ])

    def get_stdin(self, resources: Path):
        return self.context_testcase.input.get_as_string(resources)

    def get_functions(self) -> Iterable[FunctionCall]:
        return flatten(x.get_functions() for x in self.testcases)


@dataclass
class Tab(WithFeatures, WithFunctions):
    """Represents a tab on Dodona."""

    name: str
    contexts: List[Context]

    def get_used_features(self) -> FeatureSet:
        return combine_features(x.get_used_features() for x in self.contexts)

    def get_functions(self) -> Iterable[FunctionCall]:
        return flatten(x.get_functions() for x in self.contexts)


class ExecutionMode(str, Enum):
    PRECOMPILATION = "precompilation"
    INDIVIDUAL = "individual"


@dataclass
class Configuration:
    """
    The configuration options for the judge.
    """
    parallel: bool = True
    """
    Indicate that the contexts should be executed in parallel. It is recommended to
    disable this for exercises that already are multithreaded. It may also be worth
    investigating if the exercise is computationally heady.
    """
    mode: ExecutionMode = ExecutionMode.PRECOMPILATION
    """
    The default mode for the judge.
    """
    allow_fallback: Optional[bool] = True
    """
    Indicate if the judge should attempt individual mode if the precompilation mode
    fails. If nothing is given, the language-dependent default is used. If a boolean
    is given, this value is used, regardless of the language default.
    """
    language: Dict[str, Dict[str, Any]] = field(default_factory=dict)
    """
    Language-specific options for the judge. These depend on the language
    implementation; the judge itself does nothing with it.
    """


@dataclass
class Plan(WithFeatures, WithFunctions):
    """General test plan, which is used to run tests of some code."""
    tabs: List[Tab] = field(default_factory=list)
    namespace: str = "Main"
    configuration: Configuration = Configuration()

    def get_used_features(self) -> FeatureSet:
        """
        Get the used features in the testplan.

        For most features, the function will recurse into the testplan to get all
        features from each element individually.

        Detection of functions with optional parameters or parameters of different
        types is done on a testplan level, since we need an overview of every
        function call to do this.
        """
        function_features = _resolve_function_calls(self.get_functions())
        other_features = combine_features(x.get_used_features() for x in self.tabs)
        return combine_features([function_features, other_features])

    def get_functions(self) -> Iterable[FunctionCall]:
        return flatten(x.get_functions() for x in self.tabs)

    def config_for(self, language: str) -> dict:
        return self.configuration.language.get(language, dict())


class _FunctionSignature(NamedTuple):
    name: str
    namespace: str
    type: FunctionType

    @classmethod
    def from_call(cls, call: FunctionCall):
        return _FunctionSignature(call.name, call.namespace, call.type)


def _resolve_function_calls(function_calls: Iterable[FunctionCall]):
    """
    Determine if functions with optional parameters and/or dynamic types are used.
    This follows the following algorithm:

    1. Collect all function calls of functions with the same base signature.
       A base signature is a tuple of (name, namespace, type).
    2. For every set of the signature and collection of calls, get all parameters.
       TODO: support named parameters.
    3. Check for used features:
       1. If every function call has the same number of arguments.
       2. If every function call has the same type of arguments.

    :param function_calls:
    :return:
    """
    registry: Dict[_FunctionSignature, List[FunctionCall]] = defaultdict(list)

    fs = list(function_calls)
    assert all(x for x in fs)

    for function_call in fs:
        signature = _FunctionSignature.from_call(function_call)
        registry[signature].append(function_call)

    used_features = []
    for signature, calls in registry.items():
        # If there are default arguments, some function calls will not have the
        # same amount of arguments.
        if len(set(len(x.arguments) for x in calls)) != 1:
            used_features.append(FeatureSet(Constructs.DEFAULT_ARGUMENTS, set()))
        # Create mapping [arg position] -> arguments for each call
        argument_map: Dict[Any, List[Expression]] = defaultdict(list)
        for call in calls:
            for i, arg in enumerate(call.arguments):
                argument_map[i].append(arg)

        # All types inside the every list should be the same.
        # TODO: this has some limitations, more specifically, function calls and
        #  identifiers are not checked, as it is not known which types they are.
        type_use = []
        for arguments in argument_map.values():
            types = set()
            for argument in arguments:
                if isinstance(argument, SequenceType):
                    types.add((argument.type, argument.get_content_type()))
                elif isinstance(argument, get_args(Value)):
                    types.add(argument.type)
            type_use.append(types)
        if not all(len(x) == 1 for x in type_use):
            used_features.append(FeatureSet(
                Constructs.HETEROGENEOUS_ARGUMENTS, set()
            ))

        assert all(x for x in used_features)
    return combine_features(used_features)


class _PlanModel(BaseModel):
    __root__: Plan


def parse_test_plan(json_string) -> Plan:
    """Parse a test plan into the structures."""
    return _PlanModel.parse_raw(json_string).__root__


def generate_schema():
    """
    Generate a json schema for the serialisation type. It will be printed on stdout.
    """
    import json

    sc = _PlanModel.schema()
    sc['$schema'] = "http://json-schema.org/schema#"
    sc['title'] = "Testplan"
    print(json.dumps(sc, indent=2))


if __name__ == '__main__':
    # with open('../exercise/zoemzinnen/preparation/plan.json', 'r') as f:
    #     r = parse_test_plan(f.read())
    #     print(r)
    generate_schema()
