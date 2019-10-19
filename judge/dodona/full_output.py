from dataclasses import dataclass
from enum import Enum
from typing import Optional, List

from .common import Message, BadgeCount, Annotation, Status


@dataclass
class Test:
    """Node at depth 4 (leaf) in the test hierarchy, providing an output diff."""
    accepted: bool
    description: Optional[Message] = None
    expected: Optional[str] = None
    generated: Optional[str] = None
    messages: Optional[List[Message]] = None


@dataclass
class Testcase:
    """Node at depth 3 in the test hierarchy that provides an evaluation on tests."""
    accepted: bool
    description: Optional[Message] = None
    messages: Optional[List[Message]] = None
    tests: Optional[List[Test]] = None


@dataclass
class Context:
    """Node at depth 2 in the test hierarchy whose contained test cases depend on each other."""
    accepted: bool
    description: Optional[Message] = None
    groups: Optional[List[Testcase]] = None
    messages: Optional[List[Message]] = None


@dataclass
class Tab:
    """Node at depth 1 in the test hierarchy that represents test cases under the same tab."""
    badge_count: Optional[BadgeCount] = None
    description: Optional[str] = None
    groups: Optional[List[Context]] = None
    messages: Optional[List[Message]] = None


@dataclass
class Feedback:
    """Root node of the test hierarchy that contains all tests of the submission."""
    accepted: bool
    status: Status
    description: Optional[str] = None
    messages: Optional[List[Message]] = None
    groups: Optional[List[Tab]] = None
    annotations: Optional[List[Annotation]] = None
