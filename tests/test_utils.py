from typing import Any

import pytest

from tested.description_instance import create_description_instance
from tested.languages.config import limit_output


def test_limit_output_no_limit(pytestconfig):
    text = "aaaaa\nbbbbb\nccccc".strip()
    limited = limit_output(output=text, max_lines=3, limit_characters=17)
    assert text == limited
    assert len(limited) <= 17
    assert len(limited.splitlines()) <= 3
    limited = limit_output(output=text, max_lines=3, limit_characters=20)
    assert text == limited
    assert len(limited) <= 20
    assert len(limited.splitlines()) <= 3
    limited = limit_output(output=text, max_lines=4, limit_characters=17)
    assert text == limited
    assert len(limited) <= 17
    assert len(limited.splitlines()) <= 4
    limited = limit_output(output=text, max_lines=2, limit_characters=17)
    assert "aaaaa\n...\nccccc" == limited
    assert len(limited) <= 17
    assert len(limited.splitlines()) <= 3
    limited = limit_output(output=text, max_lines=3, limit_characters=15)
    assert "aaaaa\n...\nccccc" == limited
    assert len(limited) <= 15
    assert len(limited.splitlines()) <= 3
    limited = limit_output(output=text, max_lines=3, limit_characters=12)
    assert "aaaaa\n...\ncc" == limited
    assert len(limited) <= 12
    assert len(limited.splitlines()) <= 3
    limited = limit_output(output=text, max_lines=3, limit_characters=7)
    assert "a\n...\nc" == limited
    assert len(limited) <= 7
    assert len(limited.splitlines()) <= 3


@pytest.mark.parametrize(("lang", "expected"), [
    ("python", "this_is_a_function_name"), ("java", "thisIsAFunctionName"), ("c", "this_is_a_function_name"),
    ("kotlin", "thisIsAFunctionName"), ("javascript", "thisIsAFunctionName"), ("haskell", "thisIsAFunctionName"),
    ("runhaskell", "thisIsAFunctionName")
])
def test_template_function_name(lang: str, expected: str):
    template = '${function_name("this_is_a_function_name")}'
    instance = create_description_instance(template, programming_language=lang, is_html=False)
    assert instance == f"`{expected}`"


@pytest.mark.parametrize(("lang", "tested_type", "expected"), [
    # Python
    ("python", "'integer'", "int"), ("python", "'rational'", "float"), ("python", "'text'", "str"),
    ("python", '("sequence", "integer")', "List[int]"), ("python", '("array", ("set", "integer"))', "List[Set[int]]"),
    ("python", '("tuple", [("sequence", "rational"), "text"])', "Tuple[List[float], str]"),
    # Java
    ("java", "'integer'", "int"), ("java", "'rational'", "double"), ("java", "'text'", "String"),
    ("java", '("sequence", "integer")', "List<Integer>"), ("java", '("array", ("set", "integer"))', "Set<Integer>[]"),
    # c
    ("c", "'integer'", "int"), ("c", "'rational'", "double"), ("c", "'text'", "char*"),
    # Kotlin
    ("kotlin", "'integer'", "Int"), ("kotlin", "'rational'", "Double"), ("kotlin", "'text'", "String"),
    ("kotlin", '("sequence", "integer")', "List<Int>"), ("kotlin", '("array", ("set", "integer"))', "Array<Set<Int>>"),
    # JavaScript
    ("javascript", "'integer'", "number"), ("javascript", "'rational'", "number"), ("javascript", "'text'", "string"),
    ("javascript", '("sequence", "integer")', "list<number>"),
    ("javascript", '("array", ("set", "integer"))', "list<set<number>>"),
    # Haskell
    ("haskell", "'integer'", "Int"), ("haskell", "'rational'", "Double"), ("haskell", "'text'", "String"),
    ("haskell", '("sequence", "integer")', "[Int]"),
    ("haskell", '("tuple", [("sequence", "rational"), "text"])', "([Double], String)"),
])
def test_template_type_name(lang: str, tested_type: Any, expected: str):
    template = f"""${'{'}type_name({tested_type}){'}'}"""
    instance = create_description_instance(template, programming_language=lang, is_html=False)
    assert instance == f"`{expected}`"


def test_template_type_name_override():
    template = """${type_name("integer", {"java": {"integer": "long"}})}"""
    instance = create_description_instance(template, programming_language="java", is_html=False)
    assert instance == "`long`"


@pytest.mark.parametrize(("lang", "prompt"), [
    ("python", ">>>"), ("java", ">"), ("c", ">"), ("kotlin", ">"), ("javascript", ">"), ("haskell", ">")
])
def test_template_code_block_markdown(lang: str, prompt: str):
    template = """${code_start()}
${code_end()}"""
    instance = create_description_instance(template, programming_language=lang, is_html=False)
    expected = f"```console?lang={lang}&prompt={prompt}\n```"
    assert instance == expected


@pytest.mark.parametrize(("lang", "prompt"), [
    ("python", ">>>"), ("java", ">"), ("c", ">"), ("kotlin", ">"), ("javascript", ">"), ("haskell", ">")
])
def test_template_code_block_html(lang: str, prompt: str):
    template = """${code_start()}
${code_end()}"""
    instance = create_description_instance(template, programming_language=lang, is_html=True)
    expected = f'<div class="highlighter-rouge language-{lang}">\n<pre class="highlight"><code>\n</code></pre></div>'
    assert instance == expected


@pytest.mark.parametrize(("lang", "expected"), [
    pytest.param("python", ">>> random = Random()\n>>> random.new_sequence(10, 10)\n[10, 5, 2, 8, 7, 1, 3, 4, 9, 6]",
                 marks=pytest.mark.xfail),
    ("java", "> Random random = new Random();\n> random.newSequence(10, 10)\nList.of(10, 5, 2, 8, 7, 1, 3, 4, 9, 6)"),
    ("kotlin", "> var random = Random()\n> newSequence(10, 10)\nlistOf(10, 5, 2, 8, 7, 1, 3, 4, 9, 6)"),
    ("javascript", "> let random = await new Random()\n> random.newSequence(10, 10)\n[10, 5, 2, 8, 7, 1, 3, 4, 9, 6]")
])
def test_template_statement_expression(lang: str, expected: str):
    template = """${statement('random = new Random()')}
${statement('random.new_sequence(10, 10)')}
${expression('[10, 5, 2, 8, 7, 1, 3, 4, 9, 6]')}"""
    instance = create_description_instance(template, programming_language=lang, is_html=False)
    assert instance == f"{expected}"
