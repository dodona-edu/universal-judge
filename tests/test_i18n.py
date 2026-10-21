"""
Tests for the internationalization (i18n) system.

Translation file consistency and static key checks are handled by
scripts/check_i18n.py, which runs as a separate CI job.
"""

from pathlib import Path

import pytest

from tested.internationalization import get_i18n_string, set_locale
from tests.manual_utils import configuration, execute_config


@pytest.fixture(autouse=True)
def _restore_locale():
    yield
    set_locale("en")


def test_unknown_locale_falls_back_to_en():
    set_locale("xx")
    result = get_i18n_string("judge.evaluation.time-limit")
    assert result == "Time limit exceeded"


def test_recovered_output_uses_natural_language(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # Output before the first separator makes TESTed recover stdout and stderr.
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "one-with-noisy-before.tson",
        "correct",
        options={"natural_language": "en"},
    )
    result = execute_config(conf)
    assert "Standard output was:" in result
    assert "Standard error was:" in result
    assert "Standaar" not in result
