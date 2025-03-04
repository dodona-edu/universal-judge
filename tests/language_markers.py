import pytest

from tested.features import Construct
from tested.languages import LANGUAGES

ALL_LANGUAGES = LANGUAGES.keys()

def all_languages_except(*languages):
    return [l for l in ALL_LANGUAGES if l not in languages]

EXCEPTION_LANGUAGES = all_languages_except(
    "c",
    "javascript",
    "typescript",
    "runhaskell",
    "bash")

OBJECT_LANGUAGES = all_languages_except(
    "c",
    "haskell",
    "runhaskell",
    "bash")

ALL_SPECIFIC_LANGUAGES = all_languages_except("bash")

COMPILE_LANGUAGES = all_languages_except(
    "javascript",
    "typescript",
    "runhaskell",
    "bash")


SERIALIZABLE_LANGUAGES = all_languages_except("haskell")

@pytest.mark.parametrize("language", OBJECT_LANGUAGES)
def test_object_languages_support_objects(language: str):
    assert language in LANGUAGES.keys()

    config = LANGUAGES[language](None)

    assert Construct.OBJECTS in config.supported_constructs()

@pytest.mark.parametrize("language", EXCEPTION_LANGUAGES)
def test_exception_languages_support_exceptions(language: str):
    assert language in LANGUAGES.keys()

    config = LANGUAGES[language](None)

    assert Construct.EXCEPTIONS in config.supported_constructs()

def test_no_missing_languages_from_tests():
    assert sorted(ALL_LANGUAGES) == sorted(LANGUAGES.keys())
