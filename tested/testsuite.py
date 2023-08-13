"""
Structures that model the test suite in code.

This module is the authoritative source on the format and behaviour of the test suite.
When executing this module, a json-schema is generated for the format, which can be
of assistance when checking existing test suites.
"""
from collections import defaultdict
from enum import StrEnum, auto, unique
from os import path
from pathlib import Path
from typing import Any, Dict, Iterable, List, Literal, NamedTuple, Optional, Set, Union

from attrs import define, field

from tested.datatypes import BasicStringTypes
from tested.features import (
    NOTHING,
    Construct,
    FeatureSet,
    WithFeatures,
    combine_features,
)
from tested.parsing import custom_fallback_field, get_converter, ignore_field
from tested.serialisation import (
    Expression,
    FunctionCall,
    FunctionType,
    NamedArgument,
    SequenceType,
    Statement,
    Value,
    WithFunctions,
)
from tested.utils import flatten


@unique
class TextBuiltin(StrEnum):
    """Textual built in evaluators."""

    TEXT = auto()
    FILE = auto()


@unique
class ValueBuiltin(StrEnum):
    """Built in evaluators for values."""

    VALUE = auto()


@unique
class ExceptionBuiltin(StrEnum):
    """Built in evaluators for exceptions."""

    EXCEPTION = auto()


@define
class BaseBuiltinEvaluator:
    """
    A built-in evaluator in TESTed. Some basic evaluators are available, as
    enumerated by :class:`Builtin`. These are useful for things like comparing text,
    files or values.

    This is the recommended and default evaluator, since it is the least amount
    of work and the most language independent.
    """

    type: Literal["builtin"] = "builtin"
    options: Dict[str, Any] = field(factory=dict)


@define
class GenericTextEvaluator(BaseBuiltinEvaluator):
    name: TextBuiltin = TextBuiltin.TEXT


@define
class GenericValueEvaluator(BaseBuiltinEvaluator):
    name: ValueBuiltin = ValueBuiltin.VALUE


@define
class GenericExceptionEvaluator(BaseBuiltinEvaluator):
    name: ExceptionBuiltin = ExceptionBuiltin.EXCEPTION


@define
class EvaluationFunction:
    """
    An evaluation function. This not a normal function call; only the name may be
    specified, and the file should also be specified.
    """

    file: Path
    name: str = "evaluate"


@define
class ProgrammedEvaluator:
    """
    Evaluate the responses with custom code. This is still a language-independent
    method; the evaluator is run as part of the judge and receives its values from
    that judge. This type is useful, for example, when doing exercises on sequence
    alignments.
    """

    language: str
    function: EvaluationFunction
    arguments: List[Value] = field(factory=list)
    type: Literal["programmed"] = "programmed"


@define
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

    evaluators: Dict[str, EvaluationFunction] = field()
    type: Literal["specific"] = "specific"

    def for_language(self, language: str) -> EvaluationFunction:
        return self.evaluators[language]

    @evaluators.validator  # type: ignore
    def validate_evaluator(self, attribute, value):
        """There should be at least one evaluator."""
        if len(value.keys()) == 0:
            raise ValueError("At least one specific evaluator is required.")


@unique
class TextChannelType(StrEnum):
    TEXT = "text"  # Literal values
    FILE = "file"  # Path to a file


def _resolve_path(working_directory, file_path):
    """
    Resolve a path to an absolute path. Relative paths will be resolved against
    the given ``directory``, not the actual working directory.
    """
    if path.isabs(file_path):
        return path.abspath(file_path)
    else:
        return path.abspath(path.join(working_directory, file_path))


@define
class TextData(WithFeatures):
    """Describes textual data: either directly or in a file."""

    data: str
    type: TextChannelType = TextChannelType.TEXT

    def get_data_as_string(self, working_directory: Path) -> str:
        """Get the data as a string, reading the file if necessary."""
        if self.type == TextChannelType.TEXT:
            return self.data
        elif self.type == TextChannelType.FILE:
            file_path = _resolve_path(working_directory, self.data)
            with open(file_path, "r") as file:
                return file.read()
        else:
            raise AssertionError(f"Unknown enum type {self.type}")

    def get_used_features(self) -> FeatureSet:
        return NOTHING


@ignore_field(get_converter(), "show_expected")
@define
class TextOutputChannel(TextData):
    """Describes the output for textual channels."""

    evaluator: Union[GenericTextEvaluator, ProgrammedEvaluator] = field(
        factory=GenericTextEvaluator
    )


@ignore_field(get_converter(), "show_expected")
@define
class FileOutputChannel(WithFeatures):
    """Describes the output for files."""

    expected_path: str  # Path to the file to compare to.
    actual_path: str  # Path to the generated file (by the user code)
    evaluator: Union[GenericTextEvaluator, ProgrammedEvaluator] = field(
        factory=lambda: GenericTextEvaluator(name=TextBuiltin.FILE)
    )

    def get_used_features(self) -> FeatureSet:
        return NOTHING

    def get_data_as_string(self, resources: Path) -> str:
        file_path = _resolve_path(resources, self.expected_path)
        with open(file_path, "r") as file:
            return file.read()


@ignore_field(get_converter(), "show_expected")
@define
class ValueOutputChannel(WithFeatures):
    """Handles return values of function calls."""

    value: Optional[Value] = None
    evaluator: Union[
        GenericValueEvaluator, ProgrammedEvaluator, SpecificEvaluator
    ] = field(factory=GenericValueEvaluator)

    def __attrs_post_init__(self):
        if isinstance(self.evaluator, GenericValueEvaluator) and not self.value:
            raise ValueError("The generic evaluator needs an channel value.")

    def get_used_features(self) -> FeatureSet:
        if self.value:
            return self.value.get_used_features()
        return NOTHING


@define
class ExpectedException(WithFeatures):
    """
    Denotes the expected exception value.

    This is the test suite companion to the ExceptionValue from the serialization.
    """

    # If the message is none, the message will not be checked.
    message: Optional[str]
    # Dictionary of exceptions types for supported languages.
    # You can either:
    # - Specify nothing, in which case the type is not checked.
    # - Specify a dictionary mapping programming names to exception names.
    #   These exception names should already be in the right convention.
    types: Optional[Dict[str, str]] = None

    def __attrs_post_init__(self):
        if self.message is None and self.types is None:
            raise ValueError("You must specify either an exception message or type.")

    def get_used_features(self) -> FeatureSet:
        return FeatureSet({Construct.EXCEPTIONS}, types=set(), nested_types=set())

    def get_type(self, language: str) -> Optional[str]:
        if not self.types:
            return None
        return self.types.get(language)

    def readable(self, language: str) -> str:
        type_ = self.get_type(language)
        if self.message and type_:
            return f"{type_}: {self.message}"
        elif self.message:
            return self.message
        else:
            assert type_, "Cannot have neither message nor type."
            return type_


@ignore_field(get_converter(), "show_expected")
@define
class ExceptionOutputChannel(WithFeatures):
    """Handles exceptions caused by the submission."""

    exception: Optional[ExpectedException] = None
    evaluator: Union[GenericExceptionEvaluator, SpecificEvaluator] = field(
        factory=GenericExceptionEvaluator
    )

    def get_used_features(self) -> FeatureSet:
        if self.exception:
            return self.exception.get_used_features()
        return NOTHING

    def __attrs_post_init__(self):
        if isinstance(self.evaluator, GenericExceptionEvaluator) and not self.exception:
            raise ValueError("The generic evaluator needs a channel exception.")


@ignore_field(get_converter(), "show_expected")
@define
class ExitCodeOutputChannel(WithFeatures):
    """Handles exit codes."""

    value: int = 0

    def get_used_features(self) -> FeatureSet:
        return NOTHING


@unique
class EmptyChannel(WithFeatures, StrEnum):
    """There is nothing on this output channel."""

    NONE = auto()

    def get_used_features(self) -> FeatureSet:
        return NOTHING


@unique
class IgnoredChannel(WithFeatures, StrEnum):
    """A file channel is ignored by default."""

    IGNORED = auto()

    def get_used_features(self) -> FeatureSet:
        return NOTHING


SpecialOutputChannel = EmptyChannel | IgnoredChannel

EvaluatorOutputChannel = Union[
    TextOutputChannel, FileOutputChannel, ValueOutputChannel, ExceptionOutputChannel
]

NormalOutputChannel = EvaluatorOutputChannel | ExitCodeOutputChannel

OutputChannel = NormalOutputChannel | SpecialOutputChannel

TextOutput = TextOutputChannel | SpecialOutputChannel
FileOutput = FileOutputChannel | IgnoredChannel
ExceptionOutput = ExceptionOutputChannel | SpecialOutputChannel
ValueOutput = ValueOutputChannel | SpecialOutputChannel
ExitOutput = ExitCodeOutputChannel | IgnoredChannel


@define
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
            elif (
                self.exception.exception is not None and self.exception.exception.types
            ):
                languages = set(self.exception.exception.types.keys())
        if isinstance(self.result, ValueOutputChannel):
            if isinstance(self.result.evaluator, SpecificEvaluator):
                langs = set(self.result.evaluator.evaluators.keys())
                if languages is not None:
                    languages &= langs
                else:
                    languages = langs

        return languages


@define
class MainInput(WithFeatures, WithFunctions):
    """
    Input for the "main" testcase.
    """

    stdin: TextData | EmptyChannel = EmptyChannel.NONE
    arguments: List[str] = field(factory=list)
    main_call: Literal[True] = True

    def get_as_string(self, working_directory: Path) -> str:
        if self.stdin == EmptyChannel.NONE:
            return ""
        else:
            return self.stdin.get_data_as_string(working_directory)

    def get_used_features(self) -> FeatureSet:
        if self.arguments:
            return FeatureSet(set(), {BasicStringTypes.TEXT}, set())
        else:
            return NOTHING

    def get_functions(self) -> Iterable[FunctionCall]:
        return []


@define(frozen=True)
class FileUrl:
    url: str
    name: str


@ignore_field(get_converter(), "essential")
@define
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

    input: Statement | MainInput
    description: Optional[str] = None
    output: Output = field(factory=Output)
    link_files: List[FileUrl] = field(factory=list)

    def get_used_features(self) -> FeatureSet:
        return combine_features(
            [self.input.get_used_features(), self.output.get_used_features()]
        )

    def get_functions(self) -> Iterable[FunctionCall]:
        return self.input.get_functions()

    def __attrs_post_init__(self):
        # If the value test is not "None", but the input is not an expression,
        # this is an error: a statement is not an expression.
        if self.output.result != EmptyChannel.NONE and not isinstance(
            self.input, Expression
        ):
            raise ValueError("You cannot expect a value from a statement.")

    def is_main_testcase(self):
        return isinstance(self.input, MainInput)


Code = Dict[str, TextData]


@define
class Context(WithFeatures, WithFunctions):
    """
    A context is a set of dependant test cases.
    """

    testcases: List[Testcase] = field(factory=list)
    before: Code = field(factory=dict)
    after: Code = field(factory=dict)
    description: Optional[str] = None
    link_files: List[FileUrl] = field(factory=list)

    @testcases.validator  # type: ignore
    def check_testcases(self, _, value: List[Testcase]):
        # Check that only the first testcase has a main call.
        for non_first_testcase in value[1:]:
            if isinstance(non_first_testcase.input, MainInput):
                raise ValueError("Only the first testcase may have a main call.")

        # Check that only the last testcase has an exit code check.
        for non_last_testcase in value[1:-1]:
            if non_last_testcase.output.exit_code != IgnoredChannel.IGNORED:
                raise ValueError("Only the last testcase may have an exit code check.")

    def get_functions(self) -> Iterable[FunctionCall]:
        return flatten(x.get_functions() for x in self.testcases)

    def get_stdin(self, resources: Path) -> str:
        first_testcase = self.testcases[0]
        if self.has_main_testcase():
            assert isinstance(first_testcase.input, MainInput)
            return first_testcase.input.get_as_string(resources)
        else:
            return ""

    def get_used_features(self) -> FeatureSet:
        return combine_features(x.get_used_features() for x in self.testcases)

    def has_main_testcase(self):
        return isinstance(self.testcases[0].input, MainInput)

    def has_exit_testcase(self):
        return not self.testcases[-1].output.exit_code == IgnoredChannel.IGNORED


def _runs_to_tab_converter(runs: Optional[list]):
    assert isinstance(runs, list), "The field 'runs' must be a list."
    contexts = []
    for run in runs:
        if "run" in run and run["run"]["input"]["main_call"]:
            contexts.append({"testcases": [run["run"]]})
        if "contexts" in run:
            for context in run["contexts"]:
                contexts.append(context)
    return contexts


@custom_fallback_field(get_converter(), {"runs": ("contexts", _runs_to_tab_converter)})
@define
class Tab(WithFeatures, WithFunctions):
    """Represents a tab on Dodona."""

    name: str
    contexts: List[Context] = field()
    hidden: Optional[bool] = None

    def get_used_features(self) -> FeatureSet:
        assert self.contexts is not None
        return combine_features(x.get_used_features() for x in self.contexts)

    def get_functions(self) -> Iterable[FunctionCall]:
        assert self.contexts is not None
        return flatten(x.get_functions() for x in self.contexts)

    def count_contexts(self):
        assert self.contexts is not None
        return len(self.contexts)

    def __attrs_post_init__(self):
        if not self.contexts:
            raise ValueError("At least one context is required.")

    @contexts.validator  # type: ignore
    def unique_evaluation_functions(self, _, value: List[Context]):
        eval_functions: Dict[str, List[EvaluationFunction]] = defaultdict(list)

        for context in value:
            for testcase in context.testcases:
                output = testcase.output
                if isinstance(output.result, ValueOutputChannel) and isinstance(
                    output.result.evaluator, SpecificEvaluator
                ):
                    for (
                        language,
                        function,
                    ) in output.result.evaluator.evaluators.items():
                        eval_functions[language].append(function)
                if isinstance(output.exception, ExceptionOutputChannel) and isinstance(
                    output.exception.evaluator, SpecificEvaluator
                ):
                    for (
                        language,
                        function,
                    ) in output.exception.evaluator.evaluators.items():
                        eval_functions[language].append(function)

        # Check within each language that the functions are unique over the
        # files. Two calls to a function with the same name in the same file is
        # obviously allowed, as they refer to the same function.

        for language, functions in eval_functions.items():
            # Map every function name to the files it is present in.
            function_file: Dict[str, Set[Path]] = defaultdict(set)
            for function in functions:
                function_file[function.name].add(function.file)

            for function, file in function_file.items():
                if len(file) > 1:
                    raise ValueError(
                        f"Evaluator function names must be unique within the same "
                        f"run. {function} was used in multiple files: {file}"
                    )


@unique
class ExecutionMode(StrEnum):
    PRECOMPILATION = "batch"
    INDIVIDUAL = "context"


@define
class Suite(WithFeatures, WithFunctions):
    """General test suite, which is used to run tests of some code."""

    tabs: List[Tab] = field(factory=list)
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
                if isinstance(arg, NamedArgument):
                    arg = arg.value
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
                elif isinstance(argument, Value):
                    types.add(argument.type)
            type_use.append(types)
        if not all(len(x) <= 1 for x in type_use):
            used_features.append(
                FeatureSet({Construct.HETEROGENEOUS_ARGUMENTS}, set(), set())
            )

        assert all(x for x in used_features)
    return combine_features(used_features)


def parse_test_suite(json_string) -> Suite:
    """Parse a test suite into the structures."""
    from tested.parsing import parse_json_suite

    return parse_json_suite(json_string)


def generate_schema():
    """
    Generate a json schema for the serialization type. It will be printed on stdout.
    """
    # import json
    #
    # sc = Suite.model_json_schema()
    # sc["$schema"] = "http://json-schema.org/draft-07/schema#"
    # print(json.dumps(sc, indent=2))


if __name__ == "__main__":
    generate_schema()
