from pathlib import Path
from typing import Any

import pytest

from tested.descriptions import process_problem_statement
from tested.dsl.ast_translator import InvalidDslError


@pytest.mark.parametrize("language", ["python", "kotlin", "java", "haskell"])
def test_python_description(language: str):
    test_dir = Path(__file__).parent
    description_template = test_dir / "descriptions" / "example.md.jinja2"
    description_python = test_dir / "descriptions" / f"example.{language}.md"
    with open(description_template, "r") as dp:
        template = dp.read()
    with open(description_python, "r") as dp:
        expected = dp.read()
    actual = process_problem_statement(template, language)

    assert actual == expected
    assert f"This is {language}." in actual


@pytest.mark.parametrize(
    ("lang", "expected"),
    [
        ("python", "this_is_a_function_name"),
        ("java", "thisIsAFunctionName"),
        ("c", "this_is_a_function_name"),
        ("kotlin", "thisIsAFunctionName"),
        ("javascript", "thisIsAFunctionName"),
        ("haskell", "thisIsAFunctionName"),
        ("runhaskell", "thisIsAFunctionName"),
    ],
)
def test_template_function_name(lang: str, expected: str):
    template = '{{ function("this_is_a_function_name") }}'
    instance = process_problem_statement(template, lang)
    assert instance == f"{expected}"


@pytest.mark.parametrize(
    ("lang", "tested_type", "expected"),
    [
        ("python", "'integer'", "int"),
        ("python", "'real'", "float"),
        ("python", "'text'", "str"),
        ("python", '"sequence", "integer"', "list[int]"),
        ("python", '"array", ("set", ("integer", ))', "list[set[int]]"),
        (
            "python",
            '"tuple", ("sequence", ("real", )), "text"',
            "tuple[list[float], str]",
        ),
        ("java", "'integer'", "int"),
        ("java", "'real'", "double"),
        ("java", "'text'", "String"),
        ("java", '"sequence", "integer"', "List<Integer>"),
        ("java", '"array", ("set", ("integer", ))', "Set<Integer>[]"),
        ("c", "'integer'", "int"),
        ("c", "'real'", "double"),
        ("c", "'text'", "char*"),
        ("kotlin", "'integer'", "Int"),
        ("kotlin", "'real'", "Double"),
        ("kotlin", "'text'", "String"),
        ("kotlin", '"sequence", "integer"', "List<Int>"),
        ("kotlin", '"array", ("set", ("integer", ))', "Array<Set<Int>>"),
        ("javascript", "'integer'", "number"),
        ("javascript", "'real'", "number"),
        ("javascript", "'text'", "string"),
        ("javascript", '"sequence", "integer"', "array<number>"),
        ("javascript", '"array", ("set", ("integer", ))', "array<set<number>>"),
        ("haskell", "'integer'", "Int"),
        ("haskell", "'real'", "Double"),
        ("haskell", "'text'", "String"),
        ("haskell", '"sequence", "integer"', "[Int]"),
        (
            "haskell",
            '"tuple", ("sequence", ("real", )), "text"',
            "([Double], String)",
        ),
    ],
)
def test_template_type_name(lang: str, tested_type: Any, expected: str):
    template = f"""{{{{ datatype({tested_type}).simple }}}}"""
    instance = process_problem_statement(template, lang)
    assert instance == f"{expected}"


@pytest.mark.parametrize(
    ("lang", "tested_type", "expected"),
    [
        ("python", "'sequence'", "sequence"),
        ("python", "'map'", "map"),
        ("java", "'sequence'", "sequence"),
        ("java", "'map'", "map"),
        ("kotlin", "'sequence'", "sequence"),
        ("kotlin", "'map'", "map"),
        ("javascript", "'sequence'", "sequence"),
        ("javascript", "'map'", "map"),
        ("haskell", "'sequence'", "sequence"),
        ("haskell", "'list'", "list"),
    ],
)
def test_template_natural_type_name(lang: str, tested_type: Any, expected: str):
    template = f"""{{{{ datatype({tested_type}).singular }}}}"""
    instance = process_problem_statement(template, lang)
    assert instance == f"{expected}"


@pytest.mark.parametrize(
    ("lang", "tested_type", "expected"),
    [
        ("python", "'sequence'", "sequentie"),
        ("python", "'map'", "afbeelding"),
        ("java", "'sequence'", "sequentie"),
        ("java", "'map'", "afbeelding"),
        ("kotlin", "'sequence'", "sequentie"),
        ("kotlin", "'map'", "afbeelding"),
        ("javascript", "'sequence'", "sequentie"),
        ("javascript", "'map'", "afbeelding"),
        ("haskell", "'sequence'", "sequentie"),
        ("haskell", "'list'", "lijst"),
    ],
)
def test_template_natural_type_name_nl(lang: str, tested_type: Any, expected: str):
    template = f"""{{{{ datatype({tested_type}).singular }}}}"""
    instance = process_problem_statement(template, lang, "nl")
    assert instance == f"{expected}"


@pytest.mark.parametrize(
    ("lang", "prompt"),
    [
        ("python", ">>>"),
        ("java", ">"),
        ("c", ">"),
        ("kotlin", ">"),
        ("javascript", ">"),
        ("haskell", ">"),
    ],
)
def test_template_code_block_markdown(lang: str, prompt: str):
    template = """```console?lang=tested
>>> random()
5
```"""
    expected_stmt = (
        "random "
        if lang == "haskell"
        else "Submission.random()"
        if lang == "java"
        else "random()"
    )
    expected_expr = "5"
    instance = process_problem_statement(template, lang)
    expected = f"""```console?lang={lang}&prompt={prompt}
{prompt} {expected_stmt}
{expected_expr}
```"""
    assert instance == expected


@pytest.mark.parametrize(
    ("lang", "expected"),
    [
        (
            "python",
            "random = Random()\nrandom.new_sequence(10, 10)\n[10, 5, 2, 8, 7, 1, 3, 4, 9, 6]",
        ),
        (
            "java",
            "Random random = new Random()\nrandom.newSequence(10, 10)\nList.of(10, 5, 2, 8, 7, 1, 3, 4, 9, 6)",
        ),
        (
            "kotlin",
            "var random = Random()\nrandom!!.newSequence(10, 10)\nlistOf(10, 5, 2, 8, 7, 1, 3, 4, 9, 6)",
        ),
        (
            "javascript",
            "let random = new Random()\nrandom.newSequence(10, 10)\n[10, 5, 2, 8, 7, 1, 3, 4, 9, 6]",
        ),
    ],
)
def test_template_statement_expression(lang: str, expected: str):
    template = """{{ t('random = Random()') }}
{{ t('random.new_sequence(10, 10)') }}
{{ t('[10, 5, 2, 8, 7, 1, 3, 4, 9, 6]') }}"""
    instance = process_problem_statement(template, lang)
    assert instance == f"{expected}"


@pytest.mark.parametrize(
    "lang",
    [
        "python",
        "java",
        "c",
        "kotlin",
        "javascript",
        "haskell",
    ],
)
def test_template_escaped_string_code_block_markdown(lang: str):
    template = r"""```tested
"alpha\"beta\tname"
```"""
    instance = process_problem_statement(template, lang)
    expected_str = (
        "'alpha\"beta\\tname'" if lang == "python" else r'"alpha\"beta\tname"'
    )
    expected = f"""```{lang}
{expected_str}
```"""
    assert instance == expected


def test_template_failed_string():
    template = r"""```tested
> integer x = \
data(1, 2,
"alpha 
beta")
```"""
    with pytest.raises(InvalidDslError):
        process_problem_statement(template, "java")


def test_multiline_results():
    template = """
```console?lang=tested
>>> dots("paper.txt")
###..###...##..#..#.####.###..#....###.
#..#.#..#.#..#.#.#..#....#..#.#....#..#
#..#.#..#.#....##...###..###..#....#..#
###..###..#....#.#..#....#..#.#....###.
#.#..#....#..#.#.#..#....#..#.#....#.#.
#..#.#.....##..#..#.#....###..####.#..#
```
"""
    actual = process_problem_statement(template, "javascript")
    expected = """
```console?lang=javascript&prompt=>
> dots("paper.txt")
###..###...##..#..#.####.###..#....###.
#..#.#..#.#..#.#.#..#....#..#.#....#..#
#..#.#..#.#....##...###..###..#....#..#
###..###..#....#.#..#....#..#.#....###.
#.#..#....#..#.#.#..#....#..#.#....#.#.
#..#.#.....##..#..#.#....###..####.#..#
```
"""
    assert actual == expected


def test_multiline_statement():
    template = """
```console?lang=tested
>>> dots(
...  "paper.txt"
...  )
###..###...##..#..#.####.###..#....###.
#..#.#..#.#..#.#.#..#....#..#.#....#..#
#..#.#..#.#....##...###..###..#....#..#
###..###..#....#.#..#....#..#.#....###.
#.#..#....#..#.#.#..#....#..#.#....#.#.
#..#.#.....##..#..#.#....###..####.#..#
```
"""
    actual = process_problem_statement(template, "javascript")
    expected = """
```console?lang=javascript&prompt=>
> dots("paper.txt")
###..###...##..#..#.####.###..#....###.
#..#.#..#.#..#.#.#..#....#..#.#....#..#
#..#.#..#.#....##...###..###..#....#..#
###..###..#....#.#..#....#..#.#....###.
#.#..#....#..#.#.#..#....#..#.#....#.#.
#..#.#.....##..#..#.#....###..####.#..#
```
"""
    assert actual == expected


def test_long_description():
    test_dir = Path(__file__).parent
    description_template = test_dir / "descriptions" / "recoupling.md.j2"
    description_python = test_dir / "descriptions" / f"recoupling.python.md"
    with open(description_template, "r") as dp:
        template = dp.read()
    with open(description_python, "r") as dp:
        expected = dp.read()
    actual = process_problem_statement(template, "python")

    assert actual == expected
