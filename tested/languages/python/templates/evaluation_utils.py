from dataclasses import dataclass, field
from typing import List, Optional


@dataclass
class Message:
    description: str
    format: str = "text"
    permission: Optional[str] = None


@dataclass
class EvaluationResult:
    result: bool
    readable_expected: Optional[str] = None
    readable_actual: Optional[str] = None
    messages: List[Message] = field(default_factory=list)
    dsl_expected: Optional[str] = None
    dsl_actual: Optional[str] = None
