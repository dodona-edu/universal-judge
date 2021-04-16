"""
This package contains all things related to supporting programming languages. This
ranges from the templates for actually generating the code, to the actual code
needed to execute the templates.

In short, if it has to do with the templates or is programming language specific,
you will probably find it in this package.
"""
from tested.languages.c.config import C
from tested.languages.haskell.config import Haskell
from tested.languages.java.config import Java
from tested.languages.javascript.config import JavaScript
from tested.languages.kotlin.config import Kotlin
from tested.languages.python.config import Python
from tested.languages.runhaskell.config import RunHaskell
from .bash.config import Bash
from .config import Language

LANGUAGES = {
    'bash':       Bash,
    'c':          C,
    'haskell':    Haskell,
    'java':       Java,
    'javascript': JavaScript,
    'kotlin':     Kotlin,
    'python':     Python,
    'runhaskell': RunHaskell,
}


def get_language(language: str) -> Language:
    """
    Get the configuration for a programming language.
    """
    return LANGUAGES[language]()


def language_exists(language: str) -> bool:
    return language in LANGUAGES
