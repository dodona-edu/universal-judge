from tested.languages import LANGUAGES

COMPILE_LANGUAGES = [
    "python",
    "java",
    "c",
    "kotlin",
    "haskell",
    "csharp",
]
ALL_SPECIFIC_LANGUAGES = COMPILE_LANGUAGES + [
    "javascript",
    "runhaskell",
]
ALL_LANGUAGES = ALL_SPECIFIC_LANGUAGES + ["bash", "nextflow"]

EXCEPTION_LANGUAGES = ["python", "java", "kotlin", "csharp", "haskell"]


def test_no_missing_languages_from_tests():
    assert sorted(ALL_LANGUAGES) == sorted(LANGUAGES.keys())
