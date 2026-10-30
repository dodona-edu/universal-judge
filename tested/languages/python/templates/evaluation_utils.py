from dataclasses import dataclass, field


@dataclass
class Message:
    description: str
    format: str = "text"
    permission: str | None = None


@dataclass
class EvaluationResult:
    result: bool
    readable_expected: str | None = None
    readable_actual: str | None = None
    messages: list[Message] = field(default_factory=list)
    dsl_expected: str | None = None
    dsl_actual: str | None = None
