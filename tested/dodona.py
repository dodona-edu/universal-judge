"""
Class hierarchy for the partial Dodona format.

Most concepts of the format map directly to class in this module. Besides the
modeling of the data, this module provides a serializer, which writes a Dodona
update to a given output stream. See :func:`report_update`.

When running the module from the command line, a json-schema of the structure will
be printed to stdout. This might be useful to test this implementation against
the authoritative json-schema, provided by Dodona.
"""
import dataclasses
import json
from enum import Enum
from typing import Optional, Union, Literal, IO, Type

from pydantic import BaseModel
from pydantic.dataclasses import dataclass


class Permission(str, Enum):
    """To which level of user this message is visible."""
    STAFF = "staff"
    STUDENT = "student"
    ZEUS = "zeus"


@dataclass
class ExtendedMessage:
    description: str
    format: str = "text"
    permission: Optional[Permission] = None


Message = Union[ExtendedMessage, str]

BadgeCount = int

Index = int


class Severity(str, Enum):
    ERROR = "error"
    INFO = "info"
    WARNING = "warning"


class Status(str, Enum):
    COMPILATION_ERROR = "compilation error"
    CORRECT = "correct"
    MEMORY_LIMIT_EXCEEDED = "memory limit exceeded"
    RUNTIME_ERROR = "runtime error"
    TIME_LIMIT_EXCEEDED = "time limit exceeded"
    WRONG = "wrong"
    INTERNAL_ERROR = "internal error"
    OUTPUT_LIMIT_EXCEEDED = "output limit exceeded"
    # Dodona issue NOT PROCESSED STATE:
    # https://github.com/dodona-edu/dodona/issues/1785
    NOT_EXECUTED = "wrong"


@dataclass
class StatusMessage:
    """Describes the outcome of the judgement."""
    enum: Status
    human: Optional[str] = None


@dataclass
class StartJudgment:
    """Start on a new judgement."""
    command: Literal["start-judgement"] = "start-judgement"


@dataclass
class StartTab:
    """
    Start on a new tab with given title. Hidden if all contexts are accepted iff
    hidden.
    """
    title: str
    hidden: Optional[bool] = None
    command: Literal["start-tab"] = "start-tab"
    permission: Optional[Permission] = None


@dataclass
class StartContext:
    """Start on a new context."""
    description: Optional[Message] = None
    command: Literal["start-context"] = "start-context"


@dataclass
class StartTestcase:
    """Start on a new testcase with given description"""
    description: Message
    command: Literal["start-testcase"] = "start-testcase"


@dataclass
class StartTest:
    """Start on a new test with given channel answer."""
    expected: str
    channel: Optional[str] = None
    description: Optional[Message] = None
    command: Literal["start-test"] = "start-test"


@dataclass
class EscalateStatus:
    """Escalate a status for the worse."""
    status: StatusMessage
    command: Literal["escalate-status"] = "escalate-status"


@dataclass
class AppendMessage:
    """Append a message to the open object."""
    message: Message
    command: Literal["append-message"] = "append-message"


@dataclass
class AnnotateCode:
    """Annotate a piece of user_code."""
    row: Index
    text: str
    externalURL: str = None
    column: Optional[Index] = None
    type: Optional[Severity] = None
    rows: Optional[Index] = None
    columns: Optional[Index] = None
    command: Literal["annotate-code"] = "annotate-code"


@dataclass
class CloseTest:
    """
    Close the current test. Accepted iff status is correct, but you can overwrite
    this.
    """
    generated: str
    status: StatusMessage
    accepted: Optional[bool] = None
    command: Literal["close-test"] = "close-test"


@dataclass
class CloseTestcase:
    """
    Close the current testcase. Accepted iff all tests are accepted, but you can
    overwrite this.
    """
    accepted: Optional[bool] = None
    command: Literal["close-testcase"] = "close-testcase"


@dataclass
class CloseContext:
    """
    Close the current context. Accepted iff all testcases are accepted, but you can
    overwrite this.
    """
    accepted: Optional[bool] = None
    command: Literal["close-context"] = "close-context"


@dataclass
class CloseTab:
    """Close the current tab."""
    badge_count: Optional[BadgeCount] = None
    command: Literal["close-tab"] = "close-tab"


@dataclass
class CloseJudgment:
    """
    Close the current judgement. Accepted iff all contexts are accepted, status is
    the worst (highest in description) of all tests, summary is the last of all
    tests, but you can overwrite this.
    """
    accepted: Optional[bool] = None
    status: Optional[StatusMessage] = None
    command: Literal["close-judgement"] = "close-judgement"


Update = Union[
    StartJudgment, StartTab, StartContext, StartTestcase, StartTest,
    AppendMessage, AnnotateCode,
    CloseTest, CloseTestcase, CloseContext, CloseTab, CloseJudgment, EscalateStatus
]

_mapping = {
    "judgement": CloseJudgment,
    "tab":       CloseTab,
    "context":   CloseContext,
    "testcase":  CloseTestcase,
    "test":      CloseTest
}


def close_for(type_: str) -> Type[Update]:
    return _mapping[type_]


class _DodonaUpdate(BaseModel):
    __root__: Update


def _clean_dictionary(d):
    if not isinstance(d, dict):
        return d
    return {k: _clean_dictionary(v) for k, v in d.items() if v is not None}


class _EnhancedJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if dataclasses.is_dataclass(o):
            return _clean_dictionary(dataclasses.asdict(o))
        return super().default(o)


def _maybe_shorten(text: str, max_chars: int) -> str:
    if len(text) > max_chars - 3:
        text = text[:max_chars - 3] + "..."
    return text


def update_size(update: Update) -> int:
    if isinstance(update, AppendMessage):
        if isinstance(update.message, ExtendedMessage):
            return len(update.message.description.encode('utf-8'))
        else:
            assert isinstance(update.message, str)
            return len(update.message.encode('utf-8'))
    if isinstance(update, CloseTest):
        return len(update.generated.encode('utf-8'))
    if isinstance(update, AnnotateCode):
        return len(update.text.encode('utf-8'))
    return 0


def limit_size(update: Update, size: int) -> Update:
    # Handle shortening messages and other output here.
    if isinstance(update, AppendMessage):
        if isinstance(update.message, ExtendedMessage):
            shorter = _maybe_shorten(update.message.description, size)
            new_message = dataclasses.replace(update.message, description=shorter)
        else:
            assert isinstance(update.message, str)
            new_message = _maybe_shorten(update.message, size)
        update = dataclasses.replace(update, message=new_message)
    if isinstance(update, CloseTest):
        new_message = _maybe_shorten(update.generated, size)
        status = dataclasses.replace(update.status,
                                     enum=Status.OUTPUT_LIMIT_EXCEEDED)
        update = dataclasses.replace(update, generated=new_message, status=status)
    if isinstance(update, AnnotateCode):
        new_text = _maybe_shorten(update.text, size)
        update = dataclasses.replace(update, text=new_text)

    return update


def report_update(to: IO, update: Update):
    """
    Write the given update to the given output stream.

    :param to: Where to write to. Will not be closed.
    :param update: The update to write.
    """
    json.dump(update, to, cls=_EnhancedJSONEncoder)
    # noinspection PyUnreachableCode
    if __debug__:
        print("", file=to)


if __name__ == '__main__':
    sc = _DodonaUpdate.schema()
    print(sc)
