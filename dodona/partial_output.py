from dataclasses import dataclass
from enum import Enum
from typing import ClassVar, Optional

from .common import Annotation, BadgeCount, Message


class _Commands(str, Enum):
    ANNOTATE_CODE = "annotate-code"
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


class StatusEnum(str, Enum):
    COMPILATION_ERROR = "compilation error"
    CORRECT = "correct"
    CORRECT_ANSWER = "correct answer"
    INTERNAL_ERROR = "internal error"
    RUNTIME_ERROR = "runtime error"
    WRONG = "wrong"
    WRONG_ANSWER = "wrong answer"


@dataclass
class Status:
    """Describes the outcome of the judgement."""
    enum: StatusEnum
    human: Optional[str] = None


@dataclass
class _Update:
    """An update on the status of the judgment."""
    command: ClassVar[_Commands]


@dataclass
class StartJudgment(_Update):
    """Start on a new judgement."""
    command = _Commands.START_JUDGEMENT


@dataclass
class StartTab(_Update):
    """Start on a new tab with given title. Hidden if all contexts are accepted iff hidden."""
    command = _Commands.START_TAB
    title: str
    hidden: Optional[bool] = None


@dataclass
class StartContext(_Update):
    """Start on a new context."""
    command = _Commands.START_CONTEXT
    description: Optional[Message] = None


@dataclass
class StartTestcase(_Update):
    """Start on a new testcase with given description"""
    command = _Commands.START_TESTCASE
    description: Message


@dataclass
class StartTest(_Update):
    """Start on a new test with given expected answer."""
    command = _Commands.START_TEST
    expected: str
    description: Optional[Message] = None


@dataclass
class AppendMessage(_Update):
    """Append a message to the open object."""
    command = _Commands.APPEND_MESSAGE
    message: Message


@dataclass
class AnnotateCode(Annotation, _Update):
    """Annotate a piece of code."""
    command = _Commands.ANNOTATE_CODE


@dataclass
class CloseTest(_Update):
    """Close the current test. Accepted iff status is correct, but you can overwrite this."""
    command = _Commands.CLOSE_TEST
    generated: str
    status: Status
    accepted: Optional[bool] = None


@dataclass
class CloseTestcase(_Update):
    """Close the current testcase. Accepted iff all tests are accepted, but you can overwrite this."""
    command = _Commands.CLOSE_TESTCASE
    accepted: Optional[bool] = None


@dataclass
class CloseContext(_Update):
    """Close the current context. Accepted iff all testcases are accepted, but you can overwrite this."""
    command = _Commands.CLOSE_CONTEXT
    accepted: Optional[bool] = None


@dataclass
class CloseTab(_Update):
    """Close the current tab. Badgecount is the number of not-accepted tests, but you can overwrite this."""
    command = _Commands.CLOSE_TAB
    badge_count: Optional[BadgeCount] = None


@dataclass
class CloseJudgment(_Update):
    """
    Close the current judgement. Accepted iff all contexts are accepted, status is the worst (highest in description) of
    all tests, summary is the last of all tests, but you can overwrite this.
    """
    command = _Commands.CLOSE_JUDGEMENT
    accepted: Optional[bool] = None
    status: Optional[Status] = None
