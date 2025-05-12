"""
Class hierarchy for the partial Dodona format.

Most concepts of the format map directly to class in this module. Besides the
modeling of the data, this module provides a serializer, which writes a Dodona
update to a given output stream. See :func:`report_update`.

When running the module from the command line, a json-schema of the structure will
be printed to stdout. This might be useful to test this implementation against
the authoritative json-schema, provided by Dodona.
"""

import json
from enum import StrEnum, auto, unique
from typing import IO, Literal, Union

from attrs import define
from cattrs.preconf.json import make_converter


@unique
class Permission(StrEnum):
    """To which level of user this message is visible."""

    STAFF = auto()
    STUDENT = auto()
    ZEUS = auto()


@define(frozen=True)
class ExtendedMessage:
    description: str
    format: str = "text"
    permission: Permission | None = None


@define
class Metadata:
    """Currently only used for the Python tutor"""

    statements: str | None
    files: list[dict[str, str]] | None


Message = ExtendedMessage | str

BadgeCount = int

Index = int


@unique
class Severity(StrEnum):
    ERROR = auto()
    INFO = auto()
    WARNING = auto()


class Status(StrEnum):
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


@define
class StatusMessage:
    """Describes the outcome of the judgement."""

    enum: Status
    human: str | None = None


@define
class StartJudgement:
    """Start on a new judgement."""

    command: Literal["start-judgement"] = "start-judgement"


@define
class StartTab:
    """
    Start on a new tab with a given title. Hidden if all contexts are accepted
    and hidden is true.
    """

    title: str
    hidden: bool | None = None
    command: Literal["start-tab"] = "start-tab"
    permission: Permission | None = None


@define
class StartContext:
    """Start on a new context."""

    description: Message | None = None
    command: Literal["start-context"] = "start-context"


@define
class StartTestcase:
    """Start on a new testcase with a given description"""

    description: Message
    command: Literal["start-testcase"] = "start-testcase"


@define
class StartTest:
    """Start on a new test with a given channel answer."""

    expected: str
    channel: str | None = None
    description: Message | None = None
    command: Literal["start-test"] = "start-test"


@define
class EscalateStatus:
    """Escalate a status for the worse."""

    status: StatusMessage
    command: Literal["escalate-status"] = "escalate-status"


@define
class AppendMessage:
    """Append a message to the open object."""

    message: Message
    command: Literal["append-message"] = "append-message"


@define
class AnnotateCode:
    """Annotate a piece of user_code."""

    row: Index
    text: str
    externalUrl: str | None = None
    column: Index | None = None
    type: Severity | None = None
    rows: Index | None = None
    columns: Index | None = None
    command: Literal["annotate-code"] = "annotate-code"


@define
class CloseTest:
    """
    Close the current test. Accepted iff status is correct, but you can overwrite
    this.
    """

    generated: str
    status: StatusMessage
    accepted: bool | None = None
    command: Literal["close-test"] = "close-test"


@define
class CloseTestcase:
    """
    Close the current testcase. Accepted if all tests are accepted, but you can
    overwrite this.
    """

    accepted: bool | None = None
    command: Literal["close-testcase"] = "close-testcase"


@define
class CloseContext:
    """
    Close the current context. Accepted if all testcases are accepted, but you can
    overwrite this.
    """

    accepted: bool | None = None
    data: Metadata | None = None
    command: Literal["close-context"] = "close-context"


@define
class CloseTab:
    """Close the current tab."""

    badge_count: BadgeCount | None = None
    command: Literal["close-tab"] = "close-tab"


@define
class CloseJudgement:
    """
    Close the current judgement. Accepted iff all contexts are accepted, status is
    the worst (highest in description) of all tests, summary is the last of all
    tests, but you can overwrite this.
    """

    accepted: bool | None = None
    status: StatusMessage | None = None
    command: Literal["close-judgement"] = "close-judgement"


Update = Union[
    StartJudgement,
    StartTab,
    StartContext,
    StartTestcase,
    StartTest,
    AppendMessage,
    AnnotateCode,
    CloseTest,
    CloseTestcase,
    CloseContext,
    CloseTab,
    CloseJudgement,
    EscalateStatus,
]


def _clean_dictionary(d):
    if not isinstance(d, dict):
        return d
    return {k: _clean_dictionary(v) for k, v in d.items() if v is not None}


dodona_converter = make_converter()


def report_update(to: IO, update: Update):
    """
    Write the given update to the given output stream.

    :param to: Where to write to. It will not be closed.
    :param update: The update to write.
    """
    as_dict = dodona_converter.unstructure(update)
    cleaned = _clean_dictionary(as_dict)
    json.dump(cleaned, to, ensure_ascii=False)
    if __debug__:
        print("", file=to)
