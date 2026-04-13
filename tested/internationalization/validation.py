"""
Check the consistency of i18n translation files and verify that all keys used in
the source code are present in the English base translation.

Exits with a non-zero status code if any issues are found.
"""

import re
import sys
from pathlib import Path

import yaml

from tested.datatypes import AllTypes
from tested.utils import get_args

SOURCE_DIR = Path(__file__).parent.parent
I18N_DIR = Path(__file__).parent
PLURAL_KEYS = {"zero", "one", "many"}
TYPES_WITHOUT_TRANSLATION = {"unknown"}


def flatten_keys(d: dict, prefix: str = "") -> set[str]:
    """
    Return all addressable keys as dot-separated paths.
    """
    keys = set()

    for k, v in d.items():
        full = f"{prefix}.{k}" if prefix else k
        if isinstance(v, dict):
            if v.keys() <= PLURAL_KEYS:
                keys.add(full)
            keys |= flatten_keys(v, full)
        else:
            keys.add(full)
    return keys


def load_keys(locale: str) -> set[str]:
    with open(I18N_DIR / f"{locale}.yaml") as f:
        data = yaml.safe_load(f)
    return flatten_keys(data[locale])


def all_locales() -> list[str]:
    return [f.stem for f in sorted(I18N_DIR.glob("*.yaml"))]


def non_english_locales() -> list[str]:
    return [loc for loc in all_locales() if loc != "en"]


def extract_static_keys() -> list[tuple[Path, int, str]]:
    """Return (file, line, key) for all literal get_i18n_string() calls in .py files."""
    results = []
    pattern = re.compile(r'get_i18n_string\(\s*(["\'])((?:\\.|(?!\1).)+)\1')
    for path in sorted(SOURCE_DIR.rglob("*.py")):
        text = path.read_text()
        for m in pattern.finditer(text):
            line = text[: m.start()].count("\n") + 1
            results.append((path, line, m.group(2)))
    return results


def extract_template_keys() -> set[str]:
    """Return i18n keys referenced as string literals in .js/.ts template files."""
    results = set()
    pattern = re.compile(r'["\']([a-z][a-z0-9_]*(?:\.[a-z][a-z0-9_.:-]*)+)["\']')
    for path in list(SOURCE_DIR.rglob("*.ts")) + list(SOURCE_DIR.rglob("*.js")):
        for m in pattern.finditer(path.read_text()):
            results.add(m.group(1))
    return results


def extract_dynamic_prefixes() -> set[str]:
    """Return the static prefixes of f-string get_i18n_string() calls."""
    results = set()
    pattern = re.compile(r'get_i18n_string\(\s*f["\']([^{"\']+)\{')
    for path in sorted(SOURCE_DIR.rglob("*.py")):
        for m in pattern.finditer(path.read_text()):
            results.add(m.group(1))
    return results


def check_locale_consistency(en_keys: set[str]) -> list[str]:
    errors = []
    for locale in non_english_locales():
        locale_keys = load_keys(locale)
        missing = en_keys - locale_keys
        extra = locale_keys - en_keys
        if missing:
            errors.append(
                f"{locale}.yaml is missing keys from en.yaml:\n"
                + "\n".join(f"  - {k}" for k in sorted(missing))
            )
        if extra:
            errors.append(
                f"{locale}.yaml has keys not present in en.yaml:\n"
                + "\n".join(f"  - {k}" for k in sorted(extra))
            )
    return errors


def check_type_translations(en_data: dict) -> list[str]:
    """Check that every AllTypes value has a singular and plural translation."""
    all_type_values = {
        str(t) for t in get_args(AllTypes) for t in t
    } - TYPES_WITHOUT_TRANSLATION

    errors = []
    for form in ("singular", "plural"):
        yaml_keys = set(en_data["types"][form].keys())
        missing = all_type_values - yaml_keys
        extra = yaml_keys - all_type_values - TYPES_WITHOUT_TRANSLATION
        if missing:
            errors.append(
                f"types.{form} is missing entries for:\n"
                + "\n".join(f"  - {k}" for k in sorted(missing))
            )
        if extra:
            errors.append(
                f"types.{form} has entries with no matching type:\n"
                + "\n".join(f"  - {k}" for k in sorted(extra))
            )
    return errors


def check_static_keys(en_keys: set[str]) -> list[str]:
    errors = []
    for path, line, key in extract_static_keys():
        if key not in en_keys:
            rel = path.relative_to(SOURCE_DIR)
            errors.append(f"{rel}:{line}: {key!r} not found in en.yaml")
    return errors


def print_unused_keys(en_keys: set[str]) -> None:
    """Print keys in en.yaml that have no apparent reference in the source."""
    used: set[str] = {key for _, _, key in extract_static_keys()}
    used |= extract_template_keys()
    for prefix in extract_dynamic_prefixes():
        used |= {k for k in en_keys if k.startswith(prefix)}
    # types.singular.* and types.plural.* are validated by check_type_translations.
    used |= {k for k in en_keys if k.startswith(("types.singular.", "types.plural."))}

    unused = en_keys - used
    if unused:
        print(
            "Info: the following en.yaml keys have no apparent reference in the source:"
        )
        for k in sorted(unused):
            print(f"  - {k}")


def main() -> int:
    with open(I18N_DIR / "en.yaml") as f:
        en_data = yaml.safe_load(f)["en"]
    en_keys = flatten_keys(en_data)

    errors = (
        check_locale_consistency(en_keys)
        + check_type_translations(en_data)
        + check_static_keys(en_keys)
    )
    print_unused_keys(en_keys)

    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        return 1

    print(
        f"OK: checked {len(non_english_locales())} locale(s) and "
        f"{len(extract_static_keys())} static key reference(s)."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
