import pytest

COMPILE_LANGUAGES = [
    "python",
    "java",
    "c",
    "kotlin",
    pytest.param("haskell", marks=pytest.mark.haskell),
    "csharp",
]
ALL_SPECIFIC_LANGUAGES = COMPILE_LANGUAGES + [
    "javascript",
    pytest.param("runhaskell", marks=pytest.mark.haskell),
]
ALL_LANGUAGES = ALL_SPECIFIC_LANGUAGES + ["bash"]
