"""
The rule files of the ruff linter (tested/languages/python/rules, shared with
judge-pythia): ruff.toml and messages.toml agree with each other and with the
installed ruff, and the texts can be filled in.
"""

import json
import re
import subprocess
import tomllib
from functools import cache
from pathlib import Path

import pytest

from tested.languages.python.lint_messages import (
    MESSAGES_FILE,
    Messages,
    template_pattern,
)
from tested.languages.python.linter import RULES, SYNTAX_ERROR

DOCKERFILE = (
    Path(__file__).resolve().parents[1] / ".devcontainer" / "dodona-tested.dockerfile"
)
PLACEHOLDER = re.compile(r"\{(\w*)\}")
TYPES = {"error", "warning", "info"}


@cache
def messages_data() -> dict:
    with MESSAGES_FILE.open("rb") as file:
        return tomllib.load(file)


@cache
def selected() -> set[str]:
    with RULES.open("rb") as file:
        return set(tomllib.load(file)["lint"]["select"])


@cache
def ruff_formats() -> dict[str, list[str]]:
    """The message templates of every rule of the installed ruff."""
    output = subprocess.run(
        ["ruff", "rule", "--all", "--output-format=json"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    return {
        rule["code"]: rule["message_formats"]
        for rule in json.loads(output)
        if rule["code"]
    }


def entries():
    return [
        (code, entry)
        for code, rule in messages_data().items()
        for entry in rule["messages"]
    ]


def test_every_selected_rule_has_texts_and_every_text_belongs_to_a_selected_rule():
    assert selected() == set(messages_data()) - {SYNTAX_ERROR}


def test_every_rule_has_a_dodona_annotation_type():
    assert {
        code: rule["type"]
        for code, rule in messages_data().items()
        if rule["type"] not in TYPES
    } == {}


def test_the_installed_ruff_is_the_version_in_the_dockerfile():
    pinned = re.search(r"\bruff==(\S+)", DOCKERFILE.read_text())
    assert pinned is not None
    version = subprocess.run(
        ["ruff", "--version"], capture_output=True, text=True, check=True
    )
    assert version.stdout.split() == ["ruff", pinned[1]]


def known(template: str, formats: list[str]) -> bool:
    """
    A template of ruff itself, or one that spells out what a format's last
    placeholder becomes (`from {row}` written as `from line {line}`, for a better
    translation).
    """
    return template in formats or any(
        "{" in fmt
        and template.startswith(fmt[: fmt.rindex("{")])
        and len(template) > fmt.rindex("{")
        for fmt in formats
    )


@pytest.mark.parametrize(
    ("code", "entry"),
    [e for e in entries() if e[0] != SYNTAX_ERROR],
    ids=lambda x: str(x)[:40],
)
def test_every_template_is_one_the_installed_ruff_writes(code, entry):
    assert code in ruff_formats(), f"{code} is not a rule of the installed ruff"
    assert known(entry["original"], ruff_formats()[code])


@pytest.mark.parametrize(("code", "entry"), entries(), ids=lambda x: str(x)[:40])
def test_every_template_has_english_and_dutch_texts_with_only_its_own_placeholders(
    code, entry
):
    allowed = PLACEHOLDER.findall(entry["original"])
    for language in ("en", "nl"):
        text = entry.get(language)
        assert text, f"{code}: no {language} text for {entry['original']!r}"
        used = PLACEHOLDER.findall(text)
        assert set(used) <= set(allowed), f"{code} {language}: {text!r}"
        # Positional placeholders are filled in order: all of them, in the same
        # order.
        if "" in used:
            assert used.count("") == allowed.count(""), f"{code} {language}: {text!r}"


def test_no_template_is_hidden_by_an_earlier_one():
    # Messages are matched against the templates in order, so a template that an
    # earlier one also matches is never used.
    hidden = [
        (code, later["original"])
        for code, rule in messages_data().items()
        for i, earlier in enumerate(rule["messages"])
        for later in rule["messages"][i + 1 :]
        if template_pattern(earlier["original"]).fullmatch(
            PLACEHOLDER.sub("value", later["original"])
        )
    ]
    assert hidden == []


def test_every_template_matches_the_messages_it_produces():
    for code, entry in entries():
        example = PLACEHOLDER.sub("value", entry["original"])
        assert template_pattern(entry["original"]).fullmatch(example), (
            code,
            entry["original"],
        )


# The translation mechanism.


MESSAGES = Messages(
    {
        "X1": {
            "type": "info",
            "messages": [
                {
                    "original": "Name `{name}` from {row}",
                    "en": "`{name}` is from {row}",
                    "nl": "`{name}` komt van {row}",
                },
                {"original": "Swap `{}` and `{}`", "en": "Swap `{}` with `{}`"},
            ],
        }
    }
)


def test_a_message_gets_the_text_of_the_language_with_its_values():
    assert MESSAGES.text("X1", "Name `x` from line 3", "nl") == "`x` komt van line 3"


def test_a_language_without_a_text_gets_the_english_text():
    assert MESSAGES.text("X1", "Swap `a` and `b`", "nl") == "Swap `a` with `b`"


def test_a_value_with_backticks_stays_inside_its_code_span():
    # The value comes from the student's code; a single backtick would end the span.
    assert (
        MESSAGES.text("X1", "Name `a`b` from line 3", "en") == "``a`b`` is from line 3"
    )
    assert MESSAGES.text("X1", "Swap ``` and `b`", "en") == "Swap `` ` `` with `b`"


def test_a_message_without_a_matching_template_stays_as_the_linter_wrote_it():
    assert MESSAGES.text("X1", "Something else", "nl") == "Something else"
    assert MESSAGES.text("X2", "Unknown rule", "nl") == "Unknown rule"


def test_a_rule_without_texts_is_a_warning():
    assert (MESSAGES.type("X1"), MESSAGES.type("X2")) == ("info", "warning")
