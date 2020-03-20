"""
Handles testcases.
"""
from dataclasses import field
from typing import Union, List, Optional

from pydantic import root_validator
from pydantic.dataclasses import dataclass

from datatypes import StringTypes
from tested.features import FeatureSet, combine_features, Constructs, NOTHING, WithFeatures
from testplan.ast import Expression, Statement
from testplan.channels import TextData, EmptyChannel, TextOutputChannel, \
    SpecialOutputChannel, IgnoredChannel, FileOutputChannel, \
    ExceptionOutputChannel, ValueOutputChannel, ExitCodeOutputChannel


@dataclass
class ExpressionInput(WithFeatures):
    """Input for an expression."""
    expression: Expression

    def get_used_features(self) -> FeatureSet:
        return self.expression.get_used_features()


@dataclass
class StatementInput(WithFeatures):
    """Input for a statement."""
    statement: Statement

    def get_used_features(self) -> FeatureSet:
        return self.statement.get_used_features()


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


@dataclass
class Testcase(WithFeatures):
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

    def get_used_features(self) -> FeatureSet:
        return combine_features([
            self.input.get_used_features(),
            self.output.get_used_features()
        ])
