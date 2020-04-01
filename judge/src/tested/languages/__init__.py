"""
This package contains all things related to supporting programming languages. This
ranges from the templates for actually generating the code, to the actual code
needed to execute the templates.

In short, if it has to do with the templates or is programming language specific,
you will probably find it in this package.
"""
from .config import Language
from tested.languages.haskell.config import HaskellConfig
from tested.languages.java.config import JavaConfig
from tested.languages.python.config import PythonConfig
from tested.languages.c.config import CConfig


LANGUAGES = {
    'python':  PythonConfig,
    'java':    JavaConfig,
    'haskell': HaskellConfig,
    'c': CConfig
}


def get_language(language: str) -> Language:
    """
    Get the configuration for a programming language.
    """
    return LANGUAGES[language]()


def language_exists(language: str) -> bool:
    return language in LANGUAGES
