"""
Tests for the internationalization (i18n) system.

Verifies that locale files load correctly, that all keys are present across
locales, and that the English fallback works for unknown locales.
"""

from pathlib import Path

import yaml
from tested.internationalization import get_i18n_string, set_locale

I18N_DIR = Path(__file__).parent.parent / "tested" / "internationalization"


def _flatten_keys(d: dict, prefix: str = "") -> set[str]:
    """Recursively flatten a nested dict into dot-separated leaf keys."""
    keys = set()
    for k, v in d.items():
        full = f"{prefix}.{k}" if prefix else k
        if isinstance(v, dict):
            keys |= _flatten_keys(v, full)
        else:
            keys.add(full)
    return keys


def _load_keys(locale: str) -> set[str]:
    """Load a locale YAML file and return its flattened leaf keys (without the
    locale root prefix)."""
    with open(I18N_DIR / f"{locale}.yaml") as f:
        data = yaml.safe_load(f)
    # The top-level key is the locale code itself (e.g. "en", "nl", "es").
    inner = data[locale]
    return _flatten_keys(inner)


# Test that es.yaml loads and returns Spanish text.
def test_es_yaml_loadable():
    set_locale("es")
    result = get_i18n_string("judge.evaluation.time-limit")
    # Must not be the English fallback.
    assert result != "Time limit exceeded", (
        "Expected Spanish string, but got English fallback"
    )
    # Sanity-check it looks like the Spanish translation.
    assert "excedido" in result.lower() or "límite" in result.lower()


# Test that every en.yaml leaf key exists in es.yaml.
def test_all_en_keys_present_in_es():
    en_keys = _load_keys("en")
    es_keys = _load_keys("es")
    missing = en_keys - es_keys
    assert not missing, (
        f"es.yaml is missing keys present in en.yaml:\n{sorted(missing)}"
    )


# Test that every nl.yaml leaf key exists in es.yaml.
# Keys where nl.yaml and en.yaml have structurally different nesting.
# nl.yaml has oracles.programmed.student as a plain string, while en.yaml
# (and es.yaml) use the nested form oracles.programmed.student.default.
# This is a pre-existing mismatch between the two upstream files.
KNOWN_NL_STRUCTURAL_DIFFERENCES = {
    "oracles.programmed.student",
}


def test_all_nl_keys_present_in_es():
    nl_keys = _load_keys("nl")
    es_keys = _load_keys("es")
    missing = nl_keys - es_keys - KNOWN_NL_STRUCTURAL_DIFFERENCES
    assert not missing, (
        f"es.yaml is missing keys present in nl.yaml:\n{sorted(missing)}"
    )


# Test that type-related keys resolve to Spanish strings.
def test_types_resolve_to_spanish():
    set_locale("es")

    joiner = get_i18n_string("types.joiner")
    assert joiner == "o", f"Expected Spanish joiner 'o', got '{joiner}'"

    singular_int = get_i18n_string("types.singular.integer")
    assert singular_int == "entero", f"Expected 'entero', got '{singular_int}'"

    plural_int = get_i18n_string("types.plural.integer")
    assert plural_int == "enteros", f"Expected 'enteros', got '{plural_int}'"


# Test that an unknown locale falls back to English.
def test_unknown_locale_falls_back_to_en():
    set_locale("xx")
    result = get_i18n_string("judge.evaluation.time-limit")
    assert result == "Time limit exceeded", f"Expected English fallback, got '{result}'"
    # Restore to a valid locale so other tests aren't affected.
    set_locale("en")
