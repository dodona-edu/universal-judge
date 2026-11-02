import pytest

from tested.dsl.ast_translator import parse_string
from tested.languages.csharp.generators import convert_statement


@pytest.mark.parametrize(
    "source,full,expected",
    [
        ("f({1, 2})", False, "f(new HashSet<Int32>() {1, 2})"),
    ],
)
def test_convert_statement(source, full, expected):
    assert convert_statement(parse_string(source), full=full) == expected
