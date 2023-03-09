"""
Structures that model the test suite in code.

This module is the authoritative source on the format and behaviour of the test suite.
When executing this module, a json-schema is generated for the format, which can be
of assistance when checking existing test suites.
"""
from pydantic import BaseModel, root_validator, validator
from pydantic.dataclasses import dataclass

from collections import defaultdict
from dataclasses import field
from enum import Enum
from os import path
from pathlib import Path
from typing import List, Optional, Dict, Any, Literal, Union, NamedTuple, Iterable, Set
from .datatypes import BasicStringTypes
from .features import FeatureSet, combine_features, WithFeatures, NOTHING, Construct
from .serialisation import (
    ExceptionValue,
    Value,
    Expression,
    Statement,
    FunctionCall,
    SequenceType,
    FunctionType,
    WithFunctions,
)
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

    This is the recommended and default evaluator, since it is the least amount
    of work and the most language independent.
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
class EvaluationFunction:
    """
    An evaluation function. This not a normal function call; only the name may be
    specified, and the file should also be specified.
    """

    file: Path
    name: str = "evaluate"


@dataclass
class ProgrammedEvaluator:
    """
    Evaluate the responses with custom code. This is still a language-independent
    method; the evaluator is run as part of the judge and receives its values from
    that judge. This type is useful, for example, when doing exercises on sequence
    alignments.
    """

    language: str
    function: EvaluationFunction
    arguments: List[Value] = field(default_factory=list)
    type: Literal["programmed"] = "programmed"


@dataclass
class SpecificEvaluator:
    """
    Provide language-specific code that will be run in the same environment as the
    user's code. While this is very powerful and allows you to test language-specific
    constructs, there are a few caveats:

    1. The code is run alongside the user code. This means the user can potentially
       take control of the code.
    2. This will limit the context_number of language an exercise is available in,
       since you need to provide tests for all configs you want to support.
    3. It is a lot of work. You need to return the correct values, since the judge
       needs to understand what the result was.

    The code you must write should be a function that accepts the result of a user
    expression. Note: this type of evaluator is only supported when using function
    calls. If you want to evaluate_text stdout, you should use the custom evaluator
    instead.
    """

    evaluators: Dict[str, EvaluationFunction]
    type: Literal["specific"] = "specific"

    def for_language(self, language: str) -> EvaluationFunction:
        return self.evaluators[language]

    # noinspection PyMethodParameters
    @validator("evaluators")
    def validate_evaluator(cls, v):
        """There should be at least one evaluator."""

        if len(v.keys()) == 0:
            raise ValueError("At least one specific evaluator is required.")

        return v


class TextChannelType(str, Enum):
    TEXT = "text"  # Literal values
    FILE = "file"  # Path to a file


@dataclass
class TextData(WithFeatures):
    """Describes textual data: either directly or in a file."""

    data: str
    type: TextChannelType = TextChannelType.TEXT

    @staticmethod
    def _resolve_path(working_directory, file_path):
        """
        Resolve a path to an absolute path. Relative paths will be resolved against
        the given ``directory``, not the actual working directory.
        """
        if path.isabs(file_path):
            return path.abspath(file_path)
        else:
            return path.abspath(path.join(working_directory, file_path))

    def get_data_as_string(self, working_directory: Path) -> str:
        """Get the data as a string, reading the file if necessary."""
        if self.type == TextChannelType.TEXT:
            return self.data
        elif self.type == TextChannelType.FILE:
            file_path = self._resolve_path(working_directory, self.data)
            with open(file_path, "r") as file:
                return file.read()
        else:
            raise AssertionError(f"Unknown enum type {self.type}")

    def get_used_features(self) -> FeatureSet:
        return NOTHING


@dataclass
class TextOutputChannel(TextData):
    """Describes the output for textual channels."""

    evaluator: Union[GenericTextEvaluator, ProgrammedEvaluator] = field(
        default_factory=GenericTextEvaluator
    )
    show_expected: bool = True


@dataclass
class FileOutputChannel(WithFeatures):
    """Describes the output for files."""

    expected_path: str  # Path to the file to compare to.
    actual_path: str  # Path to the generated file (by the user code)
    evaluator: Union[GenericTextEvaluator, ProgrammedEvaluator] = field(
        default_factory=lambda: GenericTextEvaluator(name=TextBuiltin.FILE)
    )
    show_expected: bool = True

    def get_used_features(self) -> FeatureSet:
        return NOTHING

    def get_data_as_string(self, resources: Path) -> str:
        file_path = self._resolve_path(resources, self.expected_path)
        with open(file_path, "r") as file:
            return file.read()


@dataclass
class ValueOutputChannel(WithFeatures):
    """Handles return values of function calls."""

    value: Optional[Value] = None
    evaluator: Union[
        GenericValueEvaluator, ProgrammedEvaluator, SpecificEvaluator
    ] = field(default_factory=GenericValueEvaluator)
    show_expected: bool = True

    def get_used_features(self) -> FeatureSet:
        if self.value:
            return self.value.get_used_features()
        return NOTHING

    # noinspection PyMethodParameters
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
    evaluator: Union[GenericExceptionEvaluator, SpecificEvaluator] = field(
        default_factory=GenericExceptionEvaluator
    )
    show_expected: bool = True

    def get_used_features(self) -> FeatureSet:
        if self.exception:
            return self.exception.get_used_features()
        return NOTHING

    # noinspection PyMethodParameters
    @root_validator
    def value_requirements(cls, values):
        exception = values.get("exception")
        evaluator = values.get("evaluator")
        if isinstance(evaluator, GenericExceptionEvaluator) and not exception:
            raise ValueError("The generic evaluator needs a channel exception.")
        return values


@dataclass
class ExitCodeOutputChannel(WithFeatures):
    """Handles exit codes."""

    value: int = 0
    show_expected: bool = True

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
    ExceptionOutputChannel,
    ExitCodeOutputChannel,
]

OutputChannel = Union[NormalOutputChannel, SpecialOutputChannel]

TextOutput = Union[TextOutputChannel, SpecialOutputChannel]
FileOutput = Union[FileOutputChannel, IgnoredChannel]
ExceptionOutput = Union[ExceptionOutputChannel, SpecialOutputChannel]
ValueOutput = Union[ValueOutputChannel, SpecialOutputChannel]
ExitOutput = Union[ExitCodeOutputChannel, IgnoredChannel]


@dataclass
class Output(WithFeatures):
    """The output channels for a testcase."""

    stdout: TextOutput = EmptyChannel.NONE
    stderr: TextOutput = EmptyChannel.NONE
    file: FileOutput = IgnoredChannel.IGNORED
    exception: ExceptionOutput = EmptyChannel.NONE
    result: ValueOutput = EmptyChannel.NONE
    exit_code: ExitOutput = IgnoredChannel.IGNORED

    def get_used_features(self) -> FeatureSet:
        return combine_features(
            [
                self.stdout.get_used_features(),
                self.stderr.get_used_features(),
                self.file.get_used_features(),
                self.exception.get_used_features(),
                self.result.get_used_features(),
            ]
        )

    def get_specific_eval_languages(self) -> Optional[Set[str]]:
        """
        Get the languages supported by this output if language-specific evaluators
        are used. If none are used, None is returned, otherwise a set of languages.
        """
        languages = None
        if isinstance(self.exception, ExceptionOutputChannel):
            if isinstance(self.exception.evaluator, SpecificEvaluator):
                languages = set(self.exception.evaluator.evaluators.keys())
        if isinstance(self.result, ValueOutputChannel):
            if isinstance(self.result.evaluator, SpecificEvaluator):
                langs = set(self.result.evaluator.evaluators.keys())
                if languages is not None:
                    languages &= langs
                else:
                    languages = langs

        return languages


@dataclass
class MainInput(WithFeatures, WithFunctions):
    """
    Input for the "main" testcase.
    """

    stdin: Union[TextData, EmptyChannel] = EmptyChannel.NONE
    arguments: List[str] = field(default_factory=list)
    main_call: bool = True  # Deprecated, to remove...

    def get_as_string(self, working_directory):
        if self.stdin == EmptyChannel.NONE:
            return None
        else:
            return self.stdin.get_data_as_string(working_directory)

    def get_used_features(self) -> FeatureSet:
        if self.arguments:
            return FeatureSet(set(), {BasicStringTypes.TEXT}, set())
        else:
            return NOTHING

    def get_functions(self) -> Iterable[FunctionCall]:
        return []


@dataclass(frozen=True)
class FileUrl:
    url: str
    name: str


@dataclass
class Testcase(WithFeatures, WithFunctions):
    """
    A testcase is some input statement and a set of tests on the effects of that
    input statement.

    For example, a testcase's input might be a function call, and the tests might
    be that stdout and stderr should be empty.

    One context consists of a list of testcases, the first and last of which
    are special:

    - The first testcase may be the "main" call of the submission, and thus
      provide command line arguments and stdin.
    - The last testcase may check the exit code.

    Do note that the first and last testcase may be the same if there is only
    one testcase present.
    """

    input: Union[MainInput, Statement]
    description: Optional[str] = None
    output: Output = field(default_factory=Output)
    link_files: List[FileUrl] = field(default_factory=list)

    essential: bool = False  # Deprecated, only for backwards compatability (for now)

    def get_used_features(self) -> FeatureSet:
        return combine_features(
            [self.input.get_used_features(), self.output.get_used_features()]
        )

    def get_functions(self) -> Iterable[FunctionCall]:
        return self.input.get_functions()

    @root_validator
    def no_return_with_assignment(cls, values):
        # If the value test is not "None", but the input is not an expression,
        # this is an error: a statement is not an expression.
        output = values["output"]
        if output.result != EmptyChannel.NONE and not isinstance(
            values["input"], get_args(Expression)
        ):
            raise ValueError("You cannot expect a value from a statement.")
        return values

    def is_main_testcase(self):
        return isinstance(self.input, MainInput)


Code = Dict[str, TextData]


@dataclass
class Context(WithFeatures, WithFunctions):
    """
    A context is a set of dependant test cases.
    """

    testcases: List[Testcase] = field(default_factory=list)
    before: Code = field(default_factory=dict)
    after: Code = field(default_factory=dict)
    description: Optional[str] = None
    link_files: List[FileUrl] = field(default_factory=list)

    @validator("testcases")
    def check_testcases(cls, all_testcases: List[Testcase]) -> List[Testcase]:
        # Check that only the first testcase has a main call.
        for non_first_testcase in all_testcases[1:]:
            if isinstance(non_first_testcase.input, MainInput):
                raise ValueError("Only the first testcase may have a main call.")

        # Check that only the last testcase has an exit code check.
        for non_last_testcase in all_testcases[1:-1]:
            if non_last_testcase.output.exit_code != IgnoredChannel.IGNORED:
                raise ValueError("Only the last testcase may have an exit code check.")

        return all_testcases

    def get_functions(self) -> Iterable[FunctionCall]:
        return flatten(x.get_functions() for x in self.testcases)

    def get_stdin(self, resources: Path) -> str:
        first_testcase = self.testcases[0]
        if isinstance(first_testcase.input, MainInput):
            return first_testcase.input.get_as_string(resources)
        else:
            return ""

    def get_used_features(self) -> FeatureSet:
        return combine_features(x.get_used_features() for x in self.testcases)

    def has_main_testcase(self):
        return isinstance(self.testcases[0].input, MainInput)

    def has_exit_testcase(self):
        return not self.testcases[-1].output.exit_code == IgnoredChannel.IGNORED


@dataclass
class Tab(WithFeatures, WithFunctions):
    """Represents a tab on Dodona."""

    name: str
    # Only optional for backwards compatability.
    contexts: Optional[List[Context]] = None
    hidden: Optional[bool] = None

    # Deprecated, only for backward compatability.
    # TODO: convert this to contexts automatically.
    runs: Optional[list] = None

    def get_used_features(self) -> FeatureSet:
        return combine_features(x.get_used_features() for x in self.contexts)

    def get_functions(self) -> Iterable[FunctionCall]:
        return flatten(x.get_functions() for x in self.contexts)

    def count_contexts(self):
        return len(self.contexts)

    @root_validator(pre=True)
    def migrate_runs_to_contexts(cls, values):
        runs = values.get("runs")
        if not runs:
            return values

        if "contexts" in values and values["contexts"]:
            raise ValueError(
                "You cannot mix contexts and runs in the same tab; migrate to contexts instead."
            )

        contexts = []
        for run in runs:
            if "run" in run and run["run"]["input"]["main_call"]:
                contexts.append({"testcases": [run["run"]]})
            if "contexts" in run:
                for context in run["contexts"]:
                    contexts.append(context)

        del values["runs"]
        values["contexts"] = contexts
        return values

    @root_validator
    def must_have_contexts(cls, values):
        if "contexts" not in values or not values["contexts"]:
            raise ValueError("At least one context is required.")
        return values

    @validator("contexts")
    def unique_evaluation_functions(cls, contexts: List[Context]) -> List[Context]:
        eval_functions: Dict[str, List[EvaluationFunction]] = defaultdict(list)

        for context in contexts:  # type: Context
            for testcase in context.testcases:  # type: Testcase
                output = testcase.output
                if isinstance(output.result, ValueOutputChannel) and isinstance(
                    output.result.evaluator, SpecificEvaluator
                ):
                    # noinspection PyTypeChecker
                    for (
                        language,
                        function,
                    ) in output.result.evaluator.evaluators.items():
                        eval_functions[language].append(function)
                if isinstance(output.exception, ExceptionOutputChannel) and isinstance(
                    output.exception.evaluator, SpecificEvaluator
                ):
                    # noinspection PyTypeChecker
                    for (
                        language,
                        function,
                    ) in output.exception.evaluator.evaluators.items():
                        eval_functions[language].append(function)

        # Check within each language that the functions are unique over the
        # files. Two calls to a function with the same name in the same file is
        # obviously allowed, as they refer to the same function.

        # noinspection PyTypeChecker
        for language, functions in eval_functions.items():
            # Map every function name to the files it is present in.
            function_file: Dict[str, Set[Path]] = defaultdict(set)
            for function in functions:
                function_file[function.name].add(function.file)

            # noinspection PyTypeChecker
            for function, file in function_file.items():
                if len(file) > 1:
                    raise ValueError(
                        f"Evaluator function names must be unique within the same "
                        f"run. {function} was used in multiple files: {file}"
                    )

        return contexts


class ExecutionMode(str, Enum):
    PRECOMPILATION = "batch"
    INDIVIDUAL = "context"


@dataclass
class Suite(WithFeatures, WithFunctions):
    """General test suite, which is used to run tests of some code."""

    tabs: List[Tab] = field(default_factory=list)
    namespace: str = "submission"

    def get_used_features(self) -> FeatureSet:
        """
        Get the used features in the test suite.

        For most features, the function will recurse into the test suite to get all
        the features from each element individually.

        Detection of functions with optional parameters or parameters of different
        types is done on a test suite level, since we need an overview of every
        function call to do this.
        """
        function_features = _resolve_function_calls(self.get_functions())
        other_features = combine_features(x.get_used_features() for x in self.tabs)
        return combine_features([function_features, other_features])

    def get_functions(self) -> Iterable[FunctionCall]:
        return flatten(x.get_functions() for x in self.tabs)


class _FunctionSignature(NamedTuple):
    name: str
    namespace: str
    type: FunctionType

    @classmethod
    def from_call(cls, call: FunctionCall):
        if call.namespace is None:
            namespace_signature = ""
        elif isinstance(call.namespace, FunctionCall):
            namespace_signature = str(_FunctionSignature.from_call(call.namespace))
        else:
            namespace_signature = str(call.namespace)
        return _FunctionSignature(call.name, namespace_signature, call.type)


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
    # noinspection PyTypeChecker
    for signature, calls in registry.items():
        # If there are default arguments, some function calls will not have the
        # same number of arguments.
        if len(set(len(x.arguments) for x in calls)) != 1:
            used_features.append(
                FeatureSet({Construct.DEFAULT_PARAMETERS}, set(), set())
            )
        # Create mapping [arg position] -> arguments for each call
        argument_map: Dict[Any, List[Expression]] = defaultdict(list)
        for call in calls:
            for i, arg in enumerate(call.arguments):
                argument_map[i].append(arg)

        # All types inside the every list should be the same.
        # TODO: this has some limitations, more specifically, function calls and
        #  identifiers are not checked, as it is not known which types they are.
        type_use = []
        # noinspection PyTypeChecker
        for arguments in argument_map.values():
            types = set()
            for argument in arguments:
                if isinstance(argument, SequenceType):
                    types.add((argument.type, argument.get_content_type()))
                elif isinstance(argument, get_args(Value)):
                    types.add(argument.type)
            type_use.append(types)
        if not all(len(x) <= 1 for x in type_use):
            used_features.append(
                FeatureSet({Construct.HETEROGENEOUS_ARGUMENTS}, set(), set())
            )

        assert all(x for x in used_features)
    return combine_features(used_features)


class _SuiteModel(BaseModel):
    __root__: Suite


def parse_test_suite(json_string) -> Suite:
    """Parse a test suite into the structures."""
    return _SuiteModel.parse_raw(json_string).__root__


def generate_schema():
    """
    Generate a json schema for the serialization type. It will be printed on stdout.
    """
    import json

    sc = Suite.__pydantic_model__.schema()
    sc["$schema"] = "http://json-schema.org/draft-07/schema#"
    print(json.dumps(sc, indent=2))


if __name__ == "__main__":
    generate_schema()
