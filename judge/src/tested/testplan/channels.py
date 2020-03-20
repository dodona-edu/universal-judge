"""
Represents the output and input channels.
"""
from enum import Enum
from os import path
from typing import Union, Optional

from pydantic.dataclasses import dataclass
from pydantic import root_validator

from tested.features import FeatureSet, NOTHING, WithFeatures
from .evaluators import GenericTextEvaluator, ProgrammedEvaluator, \
    TextBuiltin, GenericValueEvaluator, SpecificEvaluator, GenericExceptionEvaluator
from .utils import TestPlanError

from tested.serialisation import Value, ExceptionValue


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
            try:
                file_path = self.__resolve_path(working_directory, self.data)
                with open(file_path, 'r') as file:
                    return file.read()
            except FileNotFoundError as e:
                raise TestPlanError(f"File not found: {e}")
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


def maybe_get_programmed_evaluator(
        output: Union[NormalOutputChannel, SpecialOutputChannel]
) -> Optional[ProgrammedEvaluator]:
    if output in (EmptyChannel.NONE, IgnoredChannel.IGNORED):
        return None

    if isinstance(output.evaluator, ProgrammedEvaluator):
        return output.evaluator

    return None
