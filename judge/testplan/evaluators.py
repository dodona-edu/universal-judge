"""
The evaluators.
"""
from dataclasses import field
from typing import Literal, Dict, Any, List
from enum import Enum
from pathlib import Path

from pydantic.dataclasses import dataclass

from serialisation import Value


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
       since you need to provide tests for all languages you want to support.
    3. It is a lot of work. You need to return the correct values, since the judge
       needs to understand what the result was.

    The code you must write should be a function that accepts the result of a user
    call. Note: this type of evaluator is only supported when using function calls.
    If you want to evaluate_text stdout you should use the custom evaluator instead.
    """
    evaluators: Dict[str, Path]
    type: Literal["specific"] = "specific"
