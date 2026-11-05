from collections.abc import Iterator
from dataclasses import dataclass

DECLARATION_KEYWORDS = {
    "class",
    "struct",
    "interface",
    "enum",
    "namespace",
    "delegate",
}

MODIFIERS = {
    "public",
    "internal",
    "private",
    "protected",
    "static",
    "abstract",
    "sealed",
    "partial",
    "readonly",
    "ref",
    "unsafe",
    "file",
    "new",
}


@dataclass(frozen=True)
class TopLevelLayout:
    # The offset after the `;` of the last leading using directive.
    usings_end: int | None
    # The offset after the `;` or `}` of the last top-level statement.
    statements_end: int | None
    has_declarations: bool


def _is_word_character(character: str) -> bool:
    return character.isalnum() or character == "_"


def _end_of_line(source: str, start: int) -> int:
    end = source.find("\n", start)
    return len(source) if end == -1 else end


def _end_of_quoted(source: str, start: int, verbatim: bool) -> int:
    escape = '""' if verbatim else "\\"
    position = start + 1
    while position < len(source):
        if source.startswith(escape, position):
            position += 2
        elif source[position] == '"':
            return position + 1
        else:
            position += 1
    return len(source)


def _end_of_string(source: str, start: int) -> int | None:
    quote = start
    while quote < len(source) and source[quote] in "$@":
        quote += 1
    if not source.startswith('"', quote):
        return None
    if "@" in source[start:quote]:
        return _end_of_quoted(source, quote, verbatim=True)
    quotes = 0
    while source.startswith('"', quote + quotes):
        quotes += 1
    if quotes == 2:
        return quote + 2
    if quotes >= 3:
        end = source.find('"' * quotes, quote + quotes)
        return len(source) if end == -1 else end + quotes
    return _end_of_quoted(source, quote, verbatim=False)


def _end_of_char(source: str, start: int) -> int:
    position = start + 1
    while position < len(source) and source[position] != "'":
        position += 2 if source[position] == "\\" else 1
    return min(position + 1, len(source))


def _tokens(source: str) -> Iterator[tuple[int, int]]:
    """The (start, end) offsets of each token, without whitespace, comments and preprocessor lines."""
    position = 0
    while position < len(source):
        character = source[position]
        if character.isspace() or character == "\ufeff":
            position += 1
            continue
        if source.startswith("//", position) or character == "#":
            position = _end_of_line(source, position)
            continue
        if source.startswith("/*", position):
            end = source.find("*/", position + 2)
            position = len(source) if end == -1 else end + 2
            continue

        string_end = _end_of_string(source, position)
        if string_end is not None:
            end = string_end
        elif character == "'":
            end = _end_of_char(source, position)
        elif _is_word_character(character):
            end = position + 1
            while end < len(source) and _is_word_character(source[end]):
                end += 1
        else:
            end = position + 1
        yield position, end
        position = end


def _items(source: str) -> list[list[tuple[int, int]]]:
    items = []
    current = []
    depth = 0
    for start, end in _tokens(source):
        current.append((start, end))
        text = source[start:end]
        if text in ("(", "[", "{"):
            depth += 1
        elif text in (")", "]", "}"):
            depth = max(depth - 1, 0)
        if depth == 0 and text in (";", "}"):
            items.append(current)
            current = []
    if current:
        items.append(current)
    return items


def _is_using_directive(words: list[str]) -> bool:
    if words[:1] == ["global"]:
        words = words[1:]
    if words[:1] != ["using"] or len(words) < 2 or words[1] == "(":
        return False
    if words[1] == "static" or "=" not in words:
        return True
    # `using A = X.Y;` is an alias, `using var a = ...;` a using declaration.
    return words.index("=") == 2


def _is_identifier(word: str) -> bool:
    return word[:1].isalpha() or word[:1] == "_"


def _after_brackets(words: list[str], start: int) -> int:
    depth = 0
    position = start
    while position < len(words):
        if words[position] == "[":
            depth += 1
        elif words[position] == "]":
            depth -= 1
        position += 1
        if depth == 0:
            break
    return position


def _is_declaration(words: list[str]) -> bool:
    position = 0
    while position < len(words):
        if words[position] == "[":
            position = _after_brackets(words, position)
        elif words[position] in MODIFIERS:
            position += 1
        else:
            break
    if position >= len(words):
        return False
    if words[position] in DECLARATION_KEYWORDS:
        return True
    following = words[position + 1] if position + 1 < len(words) else ""
    return words[position] == "record" and _is_identifier(following)


def _words(source: str, item: list[tuple[int, int]]) -> list[str]:
    return [source[start:end] for start, end in item]


def scan_top_level(source: str) -> TopLevelLayout:
    """Where the leading using directives and the top-level statements of a C# file end."""
    usings_end = None
    statements_end = None
    items = _items(source)
    index = 0
    while index < len(items) and _is_using_directive(_words(source, items[index])):
        usings_end = items[index][-1][1]
        index += 1
    while index < len(items) and not _is_declaration(_words(source, items[index])):
        statements_end = items[index][-1][1]
        index += 1
    return TopLevelLayout(
        usings_end=usings_end,
        statements_end=statements_end,
        has_declarations=index < len(items),
    )
