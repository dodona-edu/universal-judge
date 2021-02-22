from tested.languages.config import limit_output
from tested.utils import sorted_no_duplicates


def test_limit_output_no_limit():
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


def test_sort_no_duplicates():
    data = ['a', 5, 8, 3, 7, 6, 28, 'b', 5, (True, False), ('a', ('b', 'c')), 'data', 0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11,
            12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 'a', 5, 8, 3, 7, 6, 28,
            'b', 5, (True, False), ('a', ('b', 'c')), 'data', 0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
            18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32]
    expected = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                28, 29, 30, 31, 32, 'a', 'b', 'data', (True, False), ('a', ('b', 'c'))]
    result = sorted_no_duplicates(data)
    assert expected == result
