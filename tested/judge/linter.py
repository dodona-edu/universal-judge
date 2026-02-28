import logging
from typing import Literal

from attrs import define

from tested.configs import Bundle
from tested.dodona import AnnotateCode, AppendMessage, Severity
from tested.judge.collector import OutputManager

_logger = logging.getLogger(__name__)


def run_linter(bundle: Bundle, collector: OutputManager, remaining: float):
    """
    Run the linter on the submission. For the linter to run, two preconditions
    must be satisfied:

    1. The programming language supports a linter.
    2. The linter is allowed to run based on the configuration.

    :param bundle: The configuration bundle.
    :param collector: The output collector.
    :param remaining: The remaining time for the execution.
    """

    if not bundle.config.linter():
        _logger.debug("Linter is disabled.")
        return

    _logger.debug("Running linter...")

    messages, annotations = bundle.language.linter(remaining)
    annotations.sort(key=lambda a: (a.row, a.column or 0, a.text))

    for message in messages:
        collector.add(AppendMessage(message=message))
    for annotation in annotations:
        collector.add(annotation)


@define
class AnnotationLocation:
    row: int
    """Zero-based row number where the annotation starts."""
    rows: int | None = None
    """Optional, number of rows spanned by the annotation. If provided >= 1."""

    column: int | None = None
    """Zero-based column number where the annotation starts."""
    columns: int | None = None
    """Optional, number of columns spanned by the annotation. If provided >= 1."""


type IndexOffset = Literal[0, 1]


def get_linter_position(
    raw_start_row: str | int | None,
    source_offset: int,
    raw_end_row: str | int | None,
    raw_start_column: str | int | None,
    raw_end_column: str | int | None,
    row_base: int = 1,
    column_base: int = 1,
    end_row_inclusive: bool = True,
    end_column_inclusive: bool = True,
) -> AnnotationLocation:
    """
    Convert positions in source code reported by a linter to a Dodona-compatible format.

    The function has a bunch of configurations you can set, depending on what the linter returns.

    :param raw_start_row: Starting line of the annotation reported by the linter.
    :param source_offset: TESTed source offset (for rows)
    :param raw_end_row: End line of the annotation reported by the linter.
    :param raw_start_column: Starting column of the annotation reported by the linter.
    :param raw_end_column: End column of the annotation reported by the linter.
    :param row_base: If the linter reports lines 1-based or 0-based.
    :param column_base: If the linter reports columns 1-based or 0-based.
    :param end_row_inclusive: If the linter-reported end row is part of the annotation or not.
    :param end_column_inclusive: If the linter-reported end column is part of the annotation or not.
    """
    source_start_row = int(raw_start_row or row_base)

    rows = None
    if raw_end_row is not None:
        source_end_row = int(raw_end_row)

        row_adjustment = 1 if end_row_inclusive else 0
        rows = source_end_row - source_start_row + row_adjustment

        # Ignore invalid rows calculations
        rows = None if rows < 1 else rows

    source_start_column = (
        int(raw_start_column) if raw_start_column is not None else None
    )

    row = source_start_row - row_base + source_offset
    column = (
        source_start_column - column_base if source_start_column is not None else None
    )

    # Some linters use 0 in a 1-based column system to indicate no column
    if column is not None and column < 0:
        column = None

    columns = None
    if source_start_column is not None and raw_end_column is not None:
        source_end_column = int(raw_end_column)

        column_adjustment = 1 if end_column_inclusive else 0

        # If the error spans rows, the width is calculated from the start of the last row.
        if rows is not None and rows > 1:
            columns = source_end_column - column_base + column_adjustment
        else:
            columns = source_end_column - source_start_column + column_adjustment

        # Ignore invalid columns calculations
        columns = None if columns < 0 else columns

    return AnnotationLocation(row, rows, column, columns)


def annotation_from_position(
    position: AnnotationLocation,
    text: str,
    external_url: str | None = None,
    type: Severity | None = None,
) -> AnnotateCode:
    return AnnotateCode(
        row=position.row,
        column=position.column,
        rows=position.rows,
        columns=position.columns,
        text=text,
        externalUrl=external_url,
        type=type,
    )
