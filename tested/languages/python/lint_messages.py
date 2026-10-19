"""The texts of ruff's lint remarks, from rules/messages.toml (shared with judge-pythia).

For every rule the file holds its annotation type and, for each English
message template the linter uses, the text per language:

    [F841]
    type = "warning"
    [[F841.messages]]
    original = "Local variable `{name}` is assigned to but never used"
    en = "The variable `{name}` gets a value, but that value is never used"
    nl = "De variabele `{name}` krijgt een waarde, maar die wordt nergens gebruikt"

A remark's message is matched against the rule's templates; the values in its
placeholders (`{name}`, or `{}` by position) are filled in the text for the
language. Without a text in that language the English text is used, and
without a matching template the linter's own message.
"""

import re
import tomllib
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from functools import cache
from pathlib import Path

MESSAGES_FILE = Path(__file__).with_name("rules") / "messages.toml"
_PLACEHOLDER = re.compile(r"\{(\w*)\}")
DEFAULT_TYPE = "warning"


@dataclass(frozen=True)
class _Template:
    pattern: re.Pattern[str]
    texts: Mapping[str, str]  # language -> text


@dataclass(frozen=True)
class _Rule:
    type: str
    templates: Sequence[_Template]


class Messages:
    """The types and texts of the rules in a messages file."""

    def __init__(self, data: Mapping[str, object]) -> None:
        self._rules: dict[str, _Rule] = {}
        for code, rule in data.items():
            assert isinstance(rule, dict), f"{code} is not a table"
            templates = tuple(
                _Template(
                    template_pattern(entry["original"]),
                    {k: v for k, v in entry.items() if k != "original"},
                )
                for entry in rule.get("messages", ())
            )
            self._rules[code] = _Rule(rule.get("type", DEFAULT_TYPE), templates)

    @classmethod
    def load(cls, path: Path = MESSAGES_FILE) -> "Messages":
        with path.open("rb") as file:
            return cls(tomllib.load(file))

    def codes(self) -> set[str]:
        return set(self._rules)

    def type(self, code: str) -> str:
        rule = self._rules.get(code)
        return rule.type if rule else DEFAULT_TYPE

    def text(self, code: str, message: str, language: str) -> str:
        """The text of a remark in the language, or in English, or the linter's own message."""
        rule = self._rules.get(code)
        for template in rule.templates if rule else ():
            if (match := template.pattern.fullmatch(message)) is None:
                continue
            text = template.texts.get(language) or template.texts.get("en")
            return fill(text, match) if text else message
        return message


def template_pattern(template: str) -> re.Pattern[str]:
    """A pattern that matches the messages a template produces, with a group per placeholder (a `{}`
    placeholder gets group `_1`, `_2`, ... by position; a repeated name matches the same text).
    """
    parts, seen, position, last = [], set(), 0, 0
    for match in _PLACEHOLDER.finditer(template):
        parts.append(re.escape(template[last : match.start()]))
        name = match[1]
        if not name:
            position += 1
            name = f"_{position}"
        parts.append(f"(?P={name})" if name in seen else f"(?P<{name}>.+?)")
        seen.add(name)
        last = match.end()
    parts.append(re.escape(template[last:]))
    return re.compile("".join(parts), re.DOTALL)


def fill(text: str, match: re.Match[str]) -> str:
    """The text with its placeholders replaced by what they matched in the linter's message."""
    position = 0

    def value(placeholder: re.Match[str]) -> str:
        nonlocal position
        name = placeholder[1]
        if not name:
            position += 1
            name = f"_{position}"
        return match[name]

    return _PLACEHOLDER.sub(value, text)


@cache
def messages() -> Messages:
    """The judge's messages file, read once."""
    return Messages.load()
