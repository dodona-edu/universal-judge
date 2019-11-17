"""Use typed classes to communicate to Dodona."""
import dataclasses
import json
import sys

from enum import Enum
from typing import Optional, Union, ClassVar, Literal

from pydantic import BaseModel, Schema, Field
from pydantic.dataclasses import dataclass


class Permission(str, Enum):
    """To which level of user this message is visible."""
    STAFF = "staff"
    STUDENT = "student"
    ZEUS = "zeus"


@dataclass
class ExtendedMessage:
    description: str
    format: str
    permission: Optional[Permission] = None


Message = Union[ExtendedMessage, str]

BadgeCount = int

Index = int


class Severity(Enum):
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


@dataclass
class TestData:
    """This is not documented, but used nonetheless"""
    channel: Optional[str] = None


class _Commands(str, Enum):
    ANNOTATE_CODE = "annotate-user_code"
    APPEND_MESSAGE = "append-message"
    CLOSE_CONTEXT = "close-context"
    CLOSE_JUDGEMENT = "close-judgement"
    CLOSE_TAB = "close-tab"
    CLOSE_TEST = "close-test"
    CLOSE_TESTCASE = "close-testcase"
    START_CONTEXT = "start-context"
    START_JUDGEMENT = "start-judgement"
    START_TAB = "start-tab"
    START_TEST = "start-test"
    START_TESTCASE = "start-testcase"


@dataclass
class StatusMessage:
    """Describes the outcome of the judgement."""
    enum: Status
    human: Optional[str] = None


@dataclass
class StartJudgment:
    """Start on a new judgement."""
    # noinspection PyUnresolvedReferences
    command: Literal["start-judgement"] = "start-judgement"


@dataclass
class StartTab:
    """Start on a new tab with given title. Hidden if all contexts are accepted iff hidden."""
    title: str
    hidden: Optional[bool] = None
    # noinspection PyUnresolvedReferences
    command: Literal["start-tab"] = "start-tab"


@dataclass
class StartContext:
    """Start on a new context."""
    description: Optional[Message] = None
    # noinspection PyUnresolvedReferences
    command: Literal["start-context"] = "start-context"


@dataclass
class StartTestcase:
    """Start on a new testcase with given description"""
    description: Message
    # noinspection PyUnresolvedReferences
    command: Literal["start-testcase"] = "start-testcase"


@dataclass
class StartTest:
    """Start on a new test with given expected answer."""
    expected: str
    description: Optional[Message] = None
    data: Optional[TestData] = None
    # noinspection PyUnresolvedReferences
    command: Literal["start-test"] = "start-test"


@dataclass
class AppendMessage:
    """Append a message to the open object."""
    message: Message
    # noinspection PyUnresolvedReferences
    command: Literal["append-message"] = "append-message"


@dataclass
class AnnotateCode:
    """Annotate a piece of user_code."""
    row: Index
    text: str
    column: Optional[Index] = None
    type: Optional[Severity] = None
    rows: Optional[Index] = None
    columns: Optional[Index] = None
    # noinspection PyUnresolvedReferences
    command: Literal["annotate-code"] = "annotate-code"


@dataclass
class CloseTest:
    """Close the current test. Accepted iff status is correct, but you can overwrite this."""
    generated: str
    status: StatusMessage
    accepted: Optional[bool] = None
    data: Optional[TestData] = None
    # noinspection PyUnresolvedReferences
    command: Literal["close-test"] = "close-test"


@dataclass
class CloseTestcase:
    """Close the current testcase. Accepted iff all tests are accepted, but you can overwrite this."""
    accepted: Optional[bool] = None
    # noinspection PyUnresolvedReferences
    command: Literal["close-testcase"] = "close-testcase"


@dataclass
class CloseContext:
    """Close the current context. Accepted iff all testcases are accepted, but you can overwrite this."""
    accepted: Optional[bool] = None
    # noinspection PyUnresolvedReferences
    command: Literal["close-context"] = "close-context"


@dataclass
class CloseTab:
    """Close the current tab. Badgecount is the number of not-accepted tests, but you can overwrite this."""
    badge_count: Optional[BadgeCount] = None
    # noinspection PyUnresolvedReferences
    command: Literal["close-tab"] = "close-tab"


@dataclass
class CloseJudgment:
    """
    Close the current judgement. Accepted iff all contexts are accepted, status is the worst (highest in description) of
    all tests, summary is the last of all tests, but you can overwrite this.
    """
    accepted: Optional[bool] = None
    status: Optional[StatusMessage] = None
    # noinspection PyUnresolvedReferences
    command: Literal["close-judgement"] = "close-judgement"


_Update = Union[StartJudgment, StartTab, StartContext, StartTestcase, StartTest, AppendMessage, AnnotateCode,
                CloseTest, CloseTestcase, CloseContext, CloseTab, CloseJudgment]


class DodonaUpdate(BaseModel):
    __root__: _Update


def _clean_dictionary(d):
    if not isinstance(d, dict):
        return d
    return {k: _clean_dictionary(v) for k, v in d.items() if v is not None}


class _EnhancedJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if dataclasses.is_dataclass(o):
            return _clean_dictionary(dataclasses.asdict(o))
        return super().default(o)


def report_update(update: _Update):
    """
    Send an update to stdout.
    """
    json.dump(update, sys.stdout, cls=_EnhancedJSONEncoder)
    # print()


if __name__ == '__main__':
    sc = DodonaUpdate.schema()
    print(json.dumps(sc, indent=2))

    v = CloseTab()
    report_update(v)
