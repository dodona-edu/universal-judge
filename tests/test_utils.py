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
