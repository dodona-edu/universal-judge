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
    "c": "tree_sitter_c",
    "cpp": "tree_sitter_cpp",
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

_C_MAIN_PARAMETERS = {
    "c": "int argc, char** argv",
    "cpp": "int argc, char* argv[]",
}

# Unbalanced braces in preprocessor branches wrap the code after them in an ERROR.
_C_TOP_LEVEL_GROUPS = {
    "ERROR",
    "preproc_elif",
    "preproc_elifdef",
    "preproc_else",
    "preproc_if",
    "preproc_ifdef",
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


def rewrite_c_main(language: str, source: str) -> str:
    """
    Rename main in a C or C++ submission to solution_main, which TESTed calls with
    argc and argv, and end its body with a return 0, which only main has implicitly.
    Prototypes of main are renamed too. No line moves.
    """
    data = source.encode()
    edits = []
    for node in _c_top_level(parse(language, data).root_node):
        if node.type not in ("declaration", "function_definition"):
            continue
        declarator = node.child_by_field_name("declarator")
        if declarator is None or declarator.type != "function_declarator":
            continue
        name = declarator.child_by_field_name("declarator")
        if name is None or name.text != b"main" or declarator.has_error:
            continue

        edits.append(_replace(name, "solution_main"))
        parameters = declarator.child_by_field_name("parameters")
        assert parameters is not None
        declared = [p.text for p in parameters.named_children if p.type != "comment"]
        if declared in ([], [b"void"]):
            edits.append(_replace_parameters(parameters, _C_MAIN_PARAMETERS[language]))
        return_type = node.child_by_field_name("type")
        if return_type is not None and return_type.text == b"void":
            edits.append(_replace(return_type, "int"))
        body = node.child_by_field_name("body")
        if body is not None and not body.has_error:
            for block in _c_function_blocks(body):
                closing = block.end_byte - 1
                edits.append(Edit(closing, closing, "return 0;"))
    return apply_edits(data, edits)


def _c_top_level(node: Node) -> list[Node]:
    nodes = []
    for child in node.children:
        if child.type in _C_TOP_LEVEL_GROUPS:
            nodes.extend(_c_top_level(child))
        else:
            nodes.append(child)
    return nodes


def _c_function_blocks(body: Node) -> list[Node]:
    """The blocks a function ends in: a function-try-block also ends in its handlers."""
    if body.type != "try_statement":
        return [body]
    clauses = [body, *(c for c in body.children if c.type == "catch_clause")]
    blocks = [c.child_by_field_name("body") for c in clauses]
    return [b for b in blocks if b is not None]


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
