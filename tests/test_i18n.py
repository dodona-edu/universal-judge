"""
Tests for the internationalization (i18n) system.

Translation file consistency and static key checks are handled by
scripts/check_i18n.py, which runs as a separate CI job.
"""

import pytest

from tested.internationalization import get_i18n_string, set_locale


@pytest.fixture(autouse=True)
def _restore_locale():
    yield
    set_locale("en")


def test_unknown_locale_falls_back_to_en():
    set_locale("xx")
    result = get_i18n_string("judge.evaluation.time-limit")
    assert result == "Time limit exceeded"
