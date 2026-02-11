from tested.judge.linter import AnnotationLocation, get_linter_position


def test_get_linter_position_defaults():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=None,
        raw_start_column=None,
        raw_end_column=None,
    )
    assert result == AnnotationLocation(row=0, rows=None, column=None, columns=None)


def test_get_linter_position_single_line_column_1based():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=1,
        raw_start_column=1,
        raw_end_column=1,
    )
    assert result == AnnotationLocation(row=0, rows=1, column=0, columns=1)


def test_get_linter_position_single_line_column_0based():
    result = get_linter_position(
        raw_start_row=0,
        source_offset=0,
        raw_end_row=0,
        raw_start_column=0,
        raw_end_column=0,
        row_base=0,
        column_base=0,
    )
    assert result == AnnotationLocation(row=0, rows=1, column=0, columns=1)


def test_get_linter_position_exclusive_end():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=2,
        raw_start_column=1,
        raw_end_column=5,
        end_row_inclusive=False,
        end_column_inclusive=False,
    )
    assert result == AnnotationLocation(row=0, rows=1, column=0, columns=4)


def test_get_linter_position_source_offset():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=5,
        raw_end_row=None,
        raw_start_column=None,
        raw_end_column=None,
    )
    assert result.row == 5


def test_get_linter_position_multi_line_1based():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=3,
        raw_start_column=5,
        raw_end_column=10,
        row_base=1,
        column_base=1,
    )
    assert result == AnnotationLocation(row=0, rows=3, column=4, columns=10)


def test_get_linter_position_multi_line_0based():
    result = get_linter_position(
        raw_start_row=0,
        source_offset=0,
        raw_end_row=2,
        raw_start_column=5,
        raw_end_column=10,
        row_base=0,
        column_base=0,
    )
    assert result == AnnotationLocation(row=0, rows=3, column=5, columns=11)


def test_get_linter_position_invalid_rows():
    result = get_linter_position(
        raw_start_row=5, source_offset=0, raw_end_row=3, raw_start_column=None, raw_end_column=None
    )
    assert result.rows is None


def test_get_linter_position_invalid_columns():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=1,
        raw_start_column=10,
        raw_end_column=5,
    )
    assert result.columns is None


def test_get_linter_position_zero_column_in_1based():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=1,
        raw_start_column=0,
        raw_end_column=None,
    )
    assert result.column is None
    assert result.columns is None


def test_get_linter_position_string_inputs():
    result = get_linter_position(
        raw_start_row="1",
        source_offset=0,
        raw_end_row="1",
        raw_start_column="1",
        raw_end_column="1",
    )
    assert result == AnnotationLocation(row=0, rows=1, column=0, columns=1)


def test_get_linter_position_column_none_end_column_present():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=1,
        raw_start_column=None,
        raw_end_column=5,
    )
    assert result.column is None
    assert result.columns is None


def test_get_linter_position_none_start_row():
    result = get_linter_position(
        raw_start_row=None,
        source_offset=0,
        raw_end_row=1,
        raw_start_column=1,
        raw_end_column=1,
    )
    assert result == AnnotationLocation(row=0, rows=1, column=0, columns=1)


def test_get_linter_position_exclusive_end_row_multiline():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=3,
        raw_start_column=1,
        raw_end_column=5,
        end_row_inclusive=False,
    )
    assert result == AnnotationLocation(row=0, rows=2, column=0, columns=5)


def test_get_linter_position_exclusive_end_column_multiline():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=2,
        raw_start_column=1,
        raw_end_column=5,
        end_column_inclusive=False,
    )
    assert result == AnnotationLocation(row=0, rows=2, column=0, columns=4)


def test_get_linter_position_multi_line_zero_column():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=2,
        raw_start_column=1,
        raw_end_column=0,
    )
    assert result == AnnotationLocation(row=0, rows=2, column=0, columns=None)


def test_get_linter_position_start_column_zero_1based():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=1,
        raw_start_column=2,
        raw_end_column=1,
    )
    assert result == AnnotationLocation(row=0, rows=1, column=1, columns=None)


def test_get_linter_position_multi_line_exclusive_1based():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=3,
        raw_start_column=5,
        raw_end_column=10,
        row_base=1,
        column_base=1,
        end_column_inclusive=False,
    )
    assert result == AnnotationLocation(row=0, rows=3, column=4, columns=9)


def test_get_linter_position_multi_line_large_end_column():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=3,
        raw_start_column=5,
        raw_end_column=2,
    )
    assert result == AnnotationLocation(row=0, rows=3, column=4, columns=2)


def test_get_linter_position_column_base_0_zero_start_column():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=1,
        raw_start_column=0,
        raw_end_column=5,
        column_base=0,
    )
    assert result == AnnotationLocation(row=0, rows=1, column=0, columns=6)


def test_get_linter_position_no_end_column_but_end_row():
    result = get_linter_position(
        raw_start_row=1,
        source_offset=0,
        raw_end_row=2,
        raw_start_column=1,
        raw_end_column=None,
    )
    assert result == AnnotationLocation(row=0, rows=2, column=0, columns=None)


def test_get_linter_position_start_row_base_0():
    result = get_linter_position(
        raw_start_row=0,
        source_offset=0,
        raw_end_row=None,
        raw_start_column=None,
        raw_end_column=None,
        row_base=0,
    )
    assert result.row == 0


def test_get_linter_position_start_row_base_0_with_offset():
    result = get_linter_position(
        raw_start_row=0,
        source_offset=5,
        raw_end_row=None,
        raw_start_column=None,
        raw_end_column=None,
        row_base=0,
    )
    assert result.row == 5
