from dataclasses import dataclass
from enum import Enum
from typing import Optional, Union


LANGUAGE_WITH_HIGHLIGHTING = {"python", "javascript"}


def supports_input_highlighting(language: str) -> bool:
    return language in LANGUAGE_WITH_HIGHLIGHTING


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


@dataclass
class Annotation:
    """Annotate a piece of user_code."""
    row: Index
    text: str
    column: Optional[Index] = None
    type: Optional[Severity] = None
    rows: Optional[Index] = None
    columns: Optional[Index] = None


class Status(str, Enum):
    COMPILATION_ERROR = "compilation error"
    CORRECT = "correct"
    MEMORY_LIMIT_EXCEEDED = "memory limit exceeded"
    RUNTIME_ERROR = "runtime error"
    TIME_LIMIT_EXCEEDED = "time limit exceeded"
    WRONG = "wrong"
    INTERNAL_ERROR = "internal error"
