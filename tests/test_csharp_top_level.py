import pytest

from tested.languages.csharp.top_level import TopLevelLayout, scan_top_level

STATEMENTS_ONLY = [
    ('// A classic greeting\nConsole.WriteLine("Hello");\n', "class-in-comment"),
    ('var classes = 1;\nConsole.WriteLine("Hello");\n', "class-in-identifier"),
    ('Console.WriteLine("class Foo { }");\n', "class-in-string"),
    ('Console.WriteLine(@"record ""class"" A");\n', "class-in-verbatim-string"),
    ('Console.WriteLine("a \\" class B {");\n', "class-after-escaped-quote"),
    ('Console.WriteLine($"class {1}");\n', "class-in-interpolated-string"),
    ('Console.WriteLine($@"a ""class"" {1}");\n', "class-in-interpolated-verbatim"),
    ('Console.WriteLine(@$"a ""class"" {1}");\n', "class-in-verbatim-interpolated"),
    ('Console.WriteLine("""\n    class "A" { }\n    """);\n', "class-in-raw-string"),
    ('Console.WriteLine($"""class {1}""");\n', "class-in-interpolated-raw"),
    ("char c = '{';\nchar q = '\\'';\nConsole.WriteLine(c);\n", "brace-in-char"),
    ("/* class A { } */\nConsole.WriteLine(1);\n", "class-in-block-comment"),
    ("#region class\nConsole.WriteLine(1);\n#endregion\n", "class-in-preprocessor"),
    ("static int F() => 1;\nConsole.WriteLine(F());\n", "local-function"),
    (
        'using (var r = new StringReader("a")) { Console.WriteLine(r.ReadLine()); }\n',
        "using-statement",
    ),
    ('using var r = new StringReader("a");\n', "using-declaration"),
    ('using StringReader r = new("a");\n', "using-declaration-with-type"),
    ("record = 5;\n", "record-as-identifier"),
    ("", "empty"),
]


@pytest.mark.parametrize(
    "source", [s for s, _ in STATEMENTS_ONLY], ids=[i for _, i in STATEMENTS_ONLY]
)
def test_statements_without_declarations(source: str):
    layout = scan_top_level(source)
    assert layout.usings_end is None
    assert layout.has_declarations is False
    last_end = max(source.rfind(";"), source.rfind("}")) + 1
    assert layout.statements_end == (last_end or None)


@pytest.mark.parametrize(
    "source",
    [
        "class Submission { static void Main() { } }\n",
        "using System;\n\npublic static class Submission\n{\n}\n",
        "namespace Foo\n{\n    class A { }\n}\n",
        "namespace Foo;\n\nclass A { }\n",
        "[Serializable]\npublic sealed class A { }\n",
        "// Console.WriteLine(1);\nenum Color { Red }\n",
        "record P(int X);\n",
        "public record struct P(int X);\n",
        "interface I { }\nstruct S { }\n",
        "﻿using System;\nclass A { }\n",
    ],
    ids=[
        "class",
        "usings-and-class",
        "namespace",
        "file-scoped-namespace",
        "attribute",
        "enum-after-comment",
        "record",
        "record-struct",
        "interface-and-struct",
        "byte-order-mark",
    ],
)
def test_declarations_only(source: str):
    layout = scan_top_level(source)
    assert layout.statements_end is None
    assert layout.has_declarations is True


@pytest.mark.parametrize(
    "source, last_using",
    [
        ("using System.Text;\n", "using System.Text;"),
        ("using static System.Math;\n", "using static System.Math;"),
        ("using Words = List<string>;\n", "using Words = List<string>;"),
        ("global using System.Text;\n", "global using System.Text;"),
        ("using A;\nusing B.C;\n", "using B.C;"),
    ],
    ids=["using", "using-static", "using-alias", "global-using", "two-usings"],
)
def test_using_directives(source: str, last_using: str):
    program = source + "\nConsole.WriteLine(1);\n"
    assert scan_top_level(program) == TopLevelLayout(
        usings_end=len(last_using) + program.index(last_using),
        statements_end=len(program) - 1,
        has_declarations=False,
    )


@pytest.mark.parametrize(
    "declaration",
    [
        "record P(int X);",
        "class A { }",
        "enum E { A }",
        "struct S { }",
        "[Obsolete]\nclass A { }",
    ],
)
def test_declarations_after_statements(declaration: str):
    statements = 'Console.WriteLine("Hello");\nif (true) { }'
    layout = scan_top_level(f"{statements}\n\n{declaration}\n")
    assert layout == TopLevelLayout(
        usings_end=None, statements_end=len(statements), has_declarations=True
    )
