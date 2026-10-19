import pytest

LANGUAGE_PARAMS = ("language", "lang", "language_and_expected")


def pytest_collection_modifyitems(items):
    for item in items:
        callspec = getattr(item, "callspec", None)
        if callspec is None:
            continue
        for name in LANGUAGE_PARAMS:
            if name in callspec.params:
                language = callspec.params[name]
                if name == "language_and_expected":
                    language = language[0]
                item.add_marker(getattr(pytest.mark, language))
