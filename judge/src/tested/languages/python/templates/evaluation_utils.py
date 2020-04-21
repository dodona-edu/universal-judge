from dataclasses import dataclass
from typing import List, Optional


@dataclass
class EvaluationResult:
    result: bool
    readable_expected: Optional[str] = None
    readable_actual: Optional[str] = None
    messages: Optional[List[str]] = None
