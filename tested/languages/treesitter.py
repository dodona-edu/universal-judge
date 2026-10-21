"""
Rewrites of submissions that need to know the structure of the source code.

The source is parsed with tree-sitter. A rewrite only happens when the part it relies
on parsed without errors: otherwise it is left alone, so the compiler reports the
actual problem.
"""

import importlib
from functools import cache
from typing import NamedTuple

from tree_sitter import Language, Node, Parser, Tree

_GRAMMARS = {
    "csharp": "tree_sitter_c_sharp",
    "haskell": "tree_sitter_haskell",
    "kotlin": "tree_sitter_kotlin",
}

_CSHARP_TYPE_DECLARATIONS = {
    "class_declaration",
    "delegate_declaration",
    "enum_declaration",
    "file_scoped_namespace_declaration",
    "interface_declaration",
    "namespace_declaration",
    "record_declaration",
    "struct_declaration",
}


class Edit(NamedTuple):
    start: int
    end: int
    text: str


class CSharpTopLevel(NamedTuple):
    usings: list[Node]
    statements: list[Node]
    declarations: list[Node]


@cache
def _parser(language: str) -> Parser:
    # Only load the grammar of the submission's language.
    grammar = importlib.import_module(_GRAMMARS[language])
    return Parser(Language(grammar.language()))


def parse(language: str, source: bytes) -> Tree:
    return _parser(language).parse(source)


def apply_edits(source: bytes, edits: list[Edit]) -> str:
    """Apply edits that do not overlap; their positions are byte offsets in source."""
    for edit in sorted(edits, reverse=True):
        source = source[: edit.start] + edit.text.encode() + source[edit.end :]
    return source.decode()


def _replace(node: Node, text: str) -> Edit:
    return Edit(node.start_byte, node.end_byte, text)


def _replace_parameters(parameters: Node, declared: str) -> Edit:
    """Replace a parameter list, keeping its newlines so no line moves."""
    assert parameters.text is not None
    newlines = "\n" * parameters.text.count(b"\n")
    return _replace(parameters, f"({declared}{newlines})")


def csharp_top_level(source: bytes) -> CSharpTopLevel | None:
    """The top-level nodes of a C# source, or None if it did not parse cleanly."""
    tree = parse("csharp", source)
    if tree.root_node.has_error:
        return None
    nodes = tree.root_node.children
    return CSharpTopLevel(
        usings=[n for n in nodes if n.type == "using_directive"],
        statements=[n for n in nodes if n.type == "global_statement"],
        declarations=[n for n in nodes if n.type in _CSHARP_TYPE_DECLARATIONS],
    )


def haskell_header_line(source: str) -> int | None:
    """
    The line where a module header goes: after the leading pragmas. None if the
    source already has a header or did not parse cleanly.
    """
    tree = parse("haskell", source.encode())
    if tree.root_node.has_error:
        return None
    line = 0
    for node in tree.root_node.children:
        if node.type == "header":
            return None
        if node.type == "pragma":
            line = node.end_point.row + 1
        elif node.type not in ("comment", "haddock"):
            break
    return line


def rewrite_kotlin_main(source: str) -> str:
    """
    Rename the top-level main function of a Kotlin submission to solutionMain, which
    TESTed calls with an Array<String>. No line moves.
    """
    data = source.encode()
    main = _kotlin_main(parse("kotlin", data))
    if main is None:
        return source
    name = main.child_by_field_name("name")
    assert name is not None
    parameters = next(c for c in main.children if c.type == "function_value_parameters")
    if parameters.has_error:
        return source

    edits = [_replace(name, "solutionMain")]
    declared = [p for p in parameters.named_children if p.type == "parameter"]
    if not declared:
        default_array = "args: Array<String> = emptyArray()"
        edits.append(_replace_parameters(parameters, default_array))
    elif any(c.type == "parameter_modifiers" for c in parameters.named_children):
        # A vararg parameter would not accept the array TESTed passes.
        parameter_name = declared[0].named_children[0].text
        assert parameter_name is not None
        declared_array = f"{parameter_name.decode()}: Array<String>"
        edits.append(_replace_parameters(parameters, declared_array))
    return apply_edits(data, edits)


def _kotlin_main(tree: Tree) -> Node | None:
    for node in tree.root_node.children:
        if node.type != "function_declaration":
            continue
        name = node.child_by_field_name("name")
        if name is not None and name.text == b"main":
            return node
    return None
